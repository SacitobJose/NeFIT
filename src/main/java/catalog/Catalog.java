package catalog;

import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.HashSet;
import java.util.AbstractMap.SimpleEntry;

import protos.Protos.CatalogRequest;
import protos.Protos.DELETENegotiation;
import protos.Protos.GETEntities;
import protos.Protos.GETEntitiesResponse;
import protos.Protos.POSTNegotiation;
import protos.Protos.Subscribe;
import protos.Protos.Unsubscribe;
import protos.Protos.GETProducerInfoResponse;
import protos.Protos.GETProducerInfo;

import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.SocketType;

/**
 * Catalog
 */
public class Catalog {
    public static void main(String[] args) throws Exception {
        // Start the ZeroMQ server
        ZContext context = new ZContext();
        ZMQ.Socket subs = context.createSocket(ZMQ.XPUB);
        subs.bind("tcp://*:8888");
        ZMQ.Socket pubs = context.createSocket(ZMQ.XSUB);
        pubs.bind("tcp://*:7777");
        ZMQ.proxy(subs, pubs, null);

        // Start the catalog server
        ServerSocket serverSocket = new ServerSocket(9999);
        HashMap<SimpleEntry<String, String>, POSTNegotiation> negotiations = new HashMap<>();
        HashSet<String> importers = new HashSet<>();
        HashSet<String> producers = new HashSet<>();
        //HashMap<SimpleEntry<String, String>, HashSet<Socket>> subscriptions = new HashMap<>();
        while (true) {
            Socket connectionSocket = serverSocket.accept();

            Thread h = new CHandler(connectionSocket, negotiations, importers, producers);
            h.start();
        }
    }
}

/**
 * Handler thread
 */
class CHandler extends Thread {
    private Socket connectionSocket;
    private HashMap<SimpleEntry<String, String>, POSTNegotiation> negotiations;
    private HashSet<String> importers;
    private HashSet<String> producers;
    //private HashMap<SimpleEntry<String, String>, HashSet<Socket>> subscriptions;

    public CHandler(Socket connectionSocket, HashMap<SimpleEntry<String, String>, POSTNegotiation> negotiations,
            HashSet<String> importers, HashSet<String> producers) {
        this.connectionSocket = connectionSocket;
        this.negotiations = negotiations;
        this.importers = importers;
        this.producers = producers;
        //this.subscriptions = subscriptions;
    }

    public void run() {
        while (true) {
            try {
                CatalogRequest cr = CatalogRequest.parseDelimitedFrom(connectionSocket.getInputStream());
                switch (cr.getRequestCase()) {
                case NN:
                    POSTNegotiation nn = cr.getNn();
                    String productName1 = nn.getProductName();
                    String producerName1 = nn.getProducerName();
                    synchronized (this.producers) {
                        this.producers.add(producerName1);
                    }

                    /*
                    HashSet<Socket> toUpdate = subscriptions.get(new SimpleEntry<>(productName1, producerName1));
                    if (toUpdate != null) {
                        for (Socket socket : toUpdate)
                            nn.writeDelimitedTo(socket.getOutputStream());
                    }
                    */

                    negotiations.put(new SimpleEntry<>(productName1, producerName1), nn);
                    break;

                case NO:
                    DELETENegotiation no = cr.getNo();
                    String productName2 = no.getProductName();
                    String producerName2 = no.getProducerName();

                    negotiations.remove(new SimpleEntry<>(productName2, producerName2));

                    break;

                case GPI:
                    GETProducerInfo pi = cr.getGpi();
                    String producerName = pi.getProducerName();
                    synchronized (this.importers) {
                        this.importers.add(pi.getUsername());
                    }

                    GETProducerInfoResponse.Builder pir = GETProducerInfoResponse.newBuilder();
                    for (SimpleEntry<String, String> key : negotiations.keySet()) {
                        String producerName3 = key.getValue();

                        if (producerName.equals(producerName3)) {
                            POSTNegotiation pn = negotiations.get(key);
                            pir.addNegotiations(pn);
                        }
                    }
                    pir.build().writeDelimitedTo(connectionSocket.getOutputStream());

                    break;

                case GE:
                    GETEntities ge = cr.getGe();
                    GETEntitiesResponse.Builder ger = GETEntitiesResponse.newBuilder();
                    if (ge.getEntities().equals(GETEntities.Type.IMPORTERS)) {
                        for (String entity : importers) {
                            ger.addEntities(entity);
                        }
                    } else {
                        for (String entity : producers) {
                            ger.addEntities(entity);
                        }
                    }

                    ger.build().writeDelimitedTo(connectionSocket.getOutputStream());
                    break;
                
                
                case SUB:
                    /*
                    Subscribe sub = cr.getSub();
                    synchronized (subscriptions) {
                        HashSet<Socket> subSockets = subscriptions
                                .get(new SimpleEntry<>(sub.getProductName(), sub.getProducerName()));
                        if (subSockets == null) {
                            subSockets = new HashSet<>();
                            subscriptions.put(new SimpleEntry<>(sub.getProductName(), sub.getProducerName()),
                                    subSockets);
                        }
                        subSockets.add(connectionSocket);
                    }
                    */
                    break;

                case UNSUB:
                    /*
                    Unsubscribe unsub = cr.getUnsub();
                    synchronized (subscriptions) {
                        HashSet<Socket> subSockets = subscriptions
                                .get(new SimpleEntry<>(unsub.getProductName(), unsub.getProducerName()));
                        if (subSockets == null)
                            break;
                        subSockets.remove(connectionSocket);
                    }
                    */
                    break;

                default: // REQUEST_NOT_SET
                    break;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
