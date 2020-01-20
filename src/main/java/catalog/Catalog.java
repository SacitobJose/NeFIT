package catalog;

import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.AbstractMap.SimpleEntry;

import protos.Protos.CatalogRequest;
import protos.Protos.POSTNegotiation;
import protos.Protos.POSTNegotiationOver;
import protos.Protos.GETProducerInfoResponse;
import protos.Protos.GETProducerInfo;

/**
 * Catalog
 */
public class Catalog {
    public static void main(String[] args) throws Exception {
        // Start the catalog server
        ServerSocket serverSocket = new ServerSocket(9999);
        HashMap<SimpleEntry<String, String>, POSTNegotiation> negotiations = new HashMap<>();
        while (true) {
            Socket connectionSocket = serverSocket.accept();

            CatalogRequest cr = CatalogRequest.parseDelimitedFrom(connectionSocket.getInputStream());
            switch (cr.getRequestCase()) {
                case NN:
                    POSTNegotiation nn = cr.getNn();
                    String productName1 = nn.getProductName();
                    String producerName1 = nn.getProducerName();

                    negotiations.put(new SimpleEntry<>(productName1, producerName1), nn);
                    break;
                
                case NO:
                    POSTNegotiationOver no = cr.getNo();
                    String productName2 = no.getProductName();
                    String producerName2 = no.getProducerName();

                    negotiations.remove(new SimpleEntry<>(productName2, producerName2));

                    break;

                case GPI:
                    GETProducerInfo pi = cr.getGpi();
                    String producerName = pi.getProducerName();

                    GETProducerInfoResponse.Builder pir = GETProducerInfoResponse.newBuilder();
                    for(SimpleEntry<String, String> key: negotiations.keySet()) {
                        String producerName3 = key.getValue();
                        
                        if (producerName.equals(producerName3)) {
                            POSTNegotiation pn =  negotiations.get(key);
                            pir.addNegotiations(pn);
                        }
                    }
                    pir.build().writeDelimitedTo(connectionSocket.getOutputStream());

                    break;

                default: // REQUEST_NOT_SET
                    break;
            }
        }
    }
}
