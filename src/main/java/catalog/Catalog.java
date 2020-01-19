package catalog;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.AbstractMap.SimpleEntry;

import protos.Protos;

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

        // Start the catalog server
        ServerSocket serverSocket = new ServerSocket(9999);
        HashMap<SimpleEntry<String, String>, POSTNegotiation> negotiations = new HashMap<>();
        while (true) {
            Socket connectionSocket = serverSocket.accept();
            BufferedReader br = new BufferedReader(new InputStreamReader(connectionSocket.getInputStream()));

            String request = br.readLine();
            // Tratar pedido
            // NewNegotiation ou NegotiationOver ou GetProducerInfo
            
            
        }
    }
}
