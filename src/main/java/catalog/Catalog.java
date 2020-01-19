package catalog;

import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.SocketType;

/**
 * Catalog
 */
public class Catalog {
    public static void main(String[] args) {
        // Start the ZeroMQ server
        ZContext context = new ZContext();
        ZMQ.Socket subs = context.createSocket(SocketType.SUB);
        ZMQ.Socket pubs = context.createSocket(SocketType.XPUB);
        pubs.bind("tcp://*:8888");
        subs.bind("tcp://*:7777");

        Thread poller = new Proxy(context, pubs, subs);
        poller.start();

        // Start the DropWizard server
        ServerSocket serverSocket = new ServerSocket(9999);
        Socket connectionSocket = serverSocket.accept();
    }
}
