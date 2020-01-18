package catalog.pubsub;

import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.SocketType;

public class Broker {

  public static void main(String[] args) {
    ZContext context = new ZContext();
    ZMQ.Socket subs = context.createSocket(SocketType.XSUB);
    ZMQ.Socket pubs = context.createSocket(SocketType.XPUB);
    pubs.bind("tcp://*:"+args[0]);
    subs.bind("tcp://*:"+args[1]);
    new Proxy(context, pubs, subs).poll();
  }
}
