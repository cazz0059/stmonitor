// lchannels - session programming in Scala
// Copyright (c) 2021, Alceste Scalas
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
/** @author Alceste Scalas <alcsc@dtu.dk> */
package lchannels

import scala.concurrent.duration.Duration

import java.net.{Socket => JSocket}
import java.util.concurrent.{LinkedTransferQueue => Fifo}

import rawhttp.core.{RawHttp, RawHttpRequest}

/** The medium of socket-based channel endpoints. */
case class Http()

/** Base class for HTTP session management and (de)serialization of messages.
 *  
 *  A "session" may span across multiple client-server connections, so the
 *  underlying socket may be updated as needed.
 */
abstract class HttpServerManager() {
  private val http = new RawHttp()
  private var queue = new Fifo[(RawHttpRequest, JSocket)]()
  private var socket: Option[JSocket] = None

  def queueRequest(r: RawHttpRequest, s: JSocket): Unit = {
    queue.add((r,s))
  }
  
  /** Turn the current HTTP request into a message object and return it.
   *  
   *  @param atMost Maximum wait time
   *  
   *  @throws java.util.concurrent.TimeoutException if after waiting for `atMost`, no message arrives
   *  @throws Exception if a deserialization error occurs.
   */
  protected[lchannels] final def destreamer(atMost: Duration): Any = {
    val (req, client) = if (atMost.isFinite) {
      val v = queue.poll(atMost.length, atMost.unit)
      if (v == null) {
        // NOTE: if a null value is received, we treat it as a timeout
        throw new java.util.concurrent.TimeoutException(f"Input timed out after ${atMost}") 
      }
      v
    } else {
      queue.take()
    }
    try {
      val ret = request(req)
      socket = Some(client)
      ret
    } catch {
      case e: Exception => {
        http.parseResponse("HTTP/1.1 500 Internal Server Error\n" +
                           "Content-Type: text/plain\n" +
                           "Content-Length: 0\n" +
                           "\n").writeTo(client.getOutputStream())
        client.close()
      }
    }
  }

  /** Convert an object into an HTTP response and send it on the current socket.
   *  
   *  @throws Exception if a serialization error occurs.
   */
  protected[lchannels] final def streamer(x: Any): Unit = {
    val res = response(x)
    val s = socket.get // We assume the socket has been set by previous request
    http.parseResponse(res).writeTo(s.getOutputStream)
    s.close()
  }
  
  /** Read the given HTTP request and return a corresponding object.
   *  
   *  @param r HTTP session to convert
   *  @throws Exception if the given request cannot be converted error occurs.
   */
  def request(r: RawHttpRequest): Any
  
  /** Convert the given object into an HTTP response (as a string)
   *  
   *  @param x Object to convert.
   */
  def response(x: Any): String
  
  /** Send a response and close the socket.
   * 
   *  @param resp The response to be sent.
   */
  def sendResponse(resp: String): Unit = {
    val s = socket.get // We assume the socket has been set by previous request
    http.parseResponse(resp).writeTo(s.getOutputStream)
    s.close()
  }

  /** Finalize the HTTP manager, closing any open socket. */
  override def finalize() = socket match {
    case Some(s) => s.close(); socket = None
    case None => ()
  }
  
  /** Create a pair of I/O socket-based channel endpoints,
   *  reading from `in` and writing to `out`.
   *  
   *  @param ec Execution context for internal `Promise`/`Future` handling
   */
  def factory[T](): (HttpServerIn[T], HttpServerOut[T]) = {
    (HttpServerIn[T](this), HttpServerOut[T](this))
  }
}

/** HTTP-based input channel endpoint (server side), usually created
 *  through the [[[HttpServerIn$.apply* companion object]]]
 *  or via [[HttpServerManager.factory]].
 */
protected[lchannels] class HttpServerIn[T](hm: HttpServerManager)
    extends medium.In[Http, T] {
  override def receive() =  {
    hm.destreamer(Duration.Inf).asInstanceOf[T]
  }
  
  override def receive(implicit atMost: Duration) = {
    try {
      hm.destreamer(atMost).asInstanceOf[T]
    } catch {
      case e: java.net.SocketTimeoutException => {
        throw new java.util.concurrent.TimeoutException(e.getMessage())
      }
    }
  }
}

/** HTTP-based input channel endpoint (server side). */
object HttpServerIn {
  /** Return a HTTP-based input channel endpoint (server side)
   * 
   * @param hm HTTP server manager handling the session
   */
  def apply[T](hm: HttpServerManager) = {
    new HttpServerIn[T](hm)
  }
}

/** HTTP-based output channel endpoint (server side), usually created
 *  through the [[[StreamOut$.apply* companion object]]]
 *  or via [[StreamManager.factory]].
 */
class HttpServerOut[-T](hm: HttpServerManager)
    extends medium.Out[Http, T] {
  override def send(x: T) = hm.streamer(x)

  override def create[U]() = hm.factory()
}

/** Stream-based output channel endpoint. */
object HttpServerOut {
  /** Return a HTTP-based output channel endpoint (server side).
   * 
   * @param hm HTTP server manager handling the session
   */
  def apply[T](hm: HttpServerManager) = {
    new HttpServerOut[T](hm)
  }
}
