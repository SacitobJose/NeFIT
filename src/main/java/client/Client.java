package client;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.Socket;
import java.nio.ByteBuffer;

import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

import protos.Protos.GETProducerInfo;
import protos.Protos.GETProducerInfoResponse;
import protos.Protos.Import;
import protos.Protos.ImporterResponse;
import protos.Protos.POSTNegotiation;
import protos.Protos.Produce;
import protos.Protos.Response;
import protos.Protos.SaleInfo;
import protos.Protos.ServerResponse;
import protos.Protos.Subscribe;
import protos.Protos.Transaction;
import protos.Protos.Unsubscribe;
import protos.Protos.Authentication;
import protos.Protos.CatalogRequest;
import protos.Protos.DealerTimeout;
import protos.Protos.GETEntities;
import protos.Protos.GETEntitiesResponse;

public class Client {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Indique o PID do terminal para output.");
            System.exit(-1);
        }
        try {
            Socket s = new Socket("localhost", 1234);
            Thread cts = new ClientToSocket(s, args[0]);
            cts.start();
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao servidor.");
        }
    }
}

class ClientToSocket extends Thread {
    InputStream is;
    OutputStream os;
    String username;
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

    String outputPID;

    Socket catalog;

    /* ZeroMQ socket for subscriptions */
    ZContext ctx = new ZContext();
    org.zeromq.ZMQ.Socket subscriptions;

    public ClientToSocket(Socket cli, String outputPID) throws IOException {
        is = cli.getInputStream();
        os = cli.getOutputStream();
        this.username = null;
        this.outputPID = outputPID;
    }

    private void clearTerminal() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

    private void waitConfirmation() throws IOException {
        System.out.print("\nClique no ENTER para continuar...");
        System.out.flush();
        this.stdin.readLine();
    }

    private int readInt() throws IOException {
        while (true) {
            try {
                return Integer.parseInt(this.stdin.readLine());
            } catch (NumberFormatException exc) {
                System.out.println("Por favor, introduza um número");
            }
        }
    }

    private void importerMenu() throws Exception {
        while (true) {
            StringBuilder main = new StringBuilder();
            main.append("O que queres fazer?\n");
            main.append("1 - Oferta de encomenda\n");
            main.append("2 - Obter negociações em curso\n");
            main.append("3 - Obter fabricantes\n");
            main.append("4 - Obter importadores\n");
            main.append("5 - Efetuar subscrição\n");
            main.append("6 - Cancelar subscrição\n");
            main.append("7 - Sair\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 7);

            switch (escolha) {
            case 1:
                Import.Builder imp = Import.newBuilder();
                imp.setImporterName(username);

                System.out.print("Nome do produto: ");
                System.out.flush();
                imp.setProductName(this.stdin.readLine());

                System.out.print("Nome de fabricante: ");
                System.out.flush();
                imp.setProducerName(this.stdin.readLine());

                System.out.print("Quantidade: ");
                System.out.flush();
                imp.setQuantity(this.readInt());

                System.out.print("Preço por produto: ");
                System.out.flush();
                imp.setUnitaryPrice(this.readInt());

                Transaction.Builder transaction = Transaction.newBuilder();
                transaction.setImport(imp.build());

                Transaction trans = transaction.build();
                byte[] size = ByteBuffer.allocate(4).putInt(trans.getSerializedSize()).array();
                this.os.write(size);
                trans.writeTo(this.os);
                this.os.flush();

                waitConfirmation();
                break;
            case 2:
                System.out.print("Fabricante: ");
                System.out.flush();

                String producer = this.stdin.readLine();
                GETProducerInfo.Builder gpi = GETProducerInfo.newBuilder();
                gpi.setUsername(username);
                gpi.setProducerName(producer);
                CatalogRequest.Builder cr = CatalogRequest.newBuilder();
                cr.setGpi(gpi);

                Socket catalog = new Socket("localhost", 9999);
                cr.build().writeDelimitedTo(catalog.getOutputStream());

                GETProducerInfoResponse gpir = GETProducerInfoResponse.parseDelimitedFrom(catalog.getInputStream());
                for (POSTNegotiation pn : gpir.getNegotiationsList()) {
                    System.out.println("\tProduto: " + pn.getProductName());
                    System.out.println("\tQuantidade máxima: " + pn.getMaximumAmount());
                    System.out.println("\tQuantidade mínima: " + pn.getMinimumAmount());
                    System.out.println("\tPreço mínimo: " + pn.getMinimumUnitaryPrice());
                    System.out.println("\tPeríodo de negociação: " + pn.getNegotiationPeriod());
                    System.out.println();
                }

                waitConfirmation();
                break;
            case 3:
                GETEntities.Builder gip = GETEntities.newBuilder();
                gip.setEntities(GETEntities.Type.PRODUCERS);

                CatalogRequest.Builder crgip = CatalogRequest.newBuilder();
                crgip.setGe(gip);
                crgip.build().writeDelimitedTo(this.catalog.getOutputStream());

                GETEntitiesResponse gerp = GETEntitiesResponse.parseDelimitedFrom(this.catalog.getInputStream());
                for (String e : gerp.getEntitiesList()) {
                    System.out.println("Fabricantes:");
                    System.out.println("\t" + e);
                }

                waitConfirmation();
                break;
            case 4:
                GETEntities.Builder gii = GETEntities.newBuilder();
                gii.setEntities(GETEntities.Type.IMPORTERS);

                CatalogRequest.Builder crgii = CatalogRequest.newBuilder();
                crgii.setGe(gii);
                crgii.build().writeDelimitedTo(this.catalog.getOutputStream());

                GETEntitiesResponse geri = GETEntitiesResponse.parseDelimitedFrom(this.catalog.getInputStream());
                for (String e : geri.getEntitiesList()) {
                    System.out.println("Importadores:");
                    System.out.println("\t" + e);
                }

                waitConfirmation();
                break;
            case 5:
                System.out.print("Fabricante: ");
                System.out.flush();

                String producer1 = this.stdin.readLine();

                System.out.print("Produto: ");
                System.out.flush();

                String product1 = this.stdin.readLine();

                subscriptions.subscribe(producer1 + product1);
                /*
                Subscribe.Builder sub = Subscribe.newBuilder();
                sub.setUsername(username);
                sub.setProducerName(producer1);
                sub.setProductName(product1);

                CatalogRequest.Builder crsub = CatalogRequest.newBuilder();
                crsub.setSub(sub);
                crsub.build().writeDelimitedTo(this.subscriptions.getOutputStream());
                */
                break;
            case 6:
                System.out.print("Fabricante: ");
                System.out.flush();

                String producer2 = this.stdin.readLine();

                System.out.print("Produto: ");
                System.out.flush();

                String product2 = this.stdin.readLine();

                subscriptions.unsubscribe(producer2 + product2);
                /*
                Unsubscribe.Builder unsub = Unsubscribe.newBuilder();
                unsub.setUsername(username);
                unsub.setProducerName(producer2);
                unsub.setProductName(product2);

                CatalogRequest.Builder crunsub = CatalogRequest.newBuilder();
                crunsub.setUnsub(unsub);
                crunsub.build().writeDelimitedTo(this.subscriptions.getOutputStream());
                */
                break;
            case 7:
                clearTerminal();
                System.exit(1);
                break;
            }
        }
    }

    private void producerMenu() throws Exception {
        while (true) {
            StringBuilder main = new StringBuilder();
            main.append("O que queres fazer?\n");
            main.append("1 - Oferta de produção\n");
            main.append("2 - Obter fabricantes\n");
            main.append("3 - Obter importadores\n");
            main.append("4 - Sair\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 4);

            switch (escolha) {
            case 1:
                Produce.Builder prod = Produce.newBuilder();

                prod.setProducerName(username);

                System.out.print("Nome do produto: ");
                System.out.flush();
                prod.setProductName(this.stdin.readLine());

                System.out.print("Quantidade mínima: ");
                System.out.flush();
                prod.setMinimumAmount(this.readInt());

                System.out.print("Quantidade máxima: ");
                System.out.flush();
                prod.setMaximumAmount(this.readInt());

                System.out.print("Preço mínimo por produto: ");
                System.out.flush();
                prod.setMinimumUnitaryPrice(this.readInt());

                System.out.print("Período de negociação (segundos): ");
                System.out.flush();
                prod.setNegotiationPeriod(this.readInt());

                Transaction.Builder transaction = Transaction.newBuilder();
                transaction.setProduce(prod.build());

                Transaction trans = transaction.build();
                byte[] size = ByteBuffer.allocate(4).putInt(trans.getSerializedSize()).array();
                this.os.write(size);
                trans.writeTo(this.os);
                this.os.flush();

                waitConfirmation();
                break;
            case 2:
                GETEntities.Builder gip = GETEntities.newBuilder();
                gip.setEntities(GETEntities.Type.PRODUCERS);

                CatalogRequest.Builder crgip = CatalogRequest.newBuilder();
                crgip.setGe(gip);
                crgip.build().writeDelimitedTo(this.catalog.getOutputStream());

                GETEntitiesResponse gerp = GETEntitiesResponse.parseDelimitedFrom(this.catalog.getInputStream());
                for (String e : gerp.getEntitiesList()) {
                    System.out.println("Fabricantes:");
                    System.out.println("\t" + e);
                }

                waitConfirmation();
                break;
            case 3:
                GETEntities.Builder gii = GETEntities.newBuilder();
                gii.setEntities(GETEntities.Type.IMPORTERS);

                CatalogRequest.Builder crgii = CatalogRequest.newBuilder();
                crgii.setGe(gii);
                crgii.build().writeDelimitedTo(this.catalog.getOutputStream());

                GETEntitiesResponse geri = GETEntitiesResponse.parseDelimitedFrom(this.catalog.getInputStream());
                for (String e : geri.getEntitiesList()) {
                    System.out.println("Importadores:");
                    System.out.println("\t" + e);
                }

                waitConfirmation();
                break;
            case 4:
                clearTerminal();
                System.exit(1);
                break;
            }
        }
    }

    public void run() {
        try {
            clearTerminal();

            String role;
            while (true) {
                Authentication.Builder auth = Authentication.newBuilder();

                System.out.print("Deseja fazer (l)ogin ou (r)egistar-se? ");
                System.out.flush();
                String method = stdin.readLine();
                if (method.equals("r"))
                    auth.setType(Authentication.AuthType.REGISTER);
                else if (method.equals("l"))
                    auth.setType(Authentication.AuthType.LOGIN);
                else {
                    System.out.println("Não é nenhum dos métodos válidos.");
                    continue;
                }

                System.out.print("É um (f)abricante ou um (i)mportador? ");
                System.out.flush();
                role = stdin.readLine();
                if (role.equals("f"))
                    auth.setUserType(Authentication.UserType.PRODUCER);
                else if (role.equals("i"))
                    auth.setUserType(Authentication.UserType.IMPORTER);
                else {
                    System.out.println("Não é nenhum dos papéis válidos.");
                    continue;
                }

                System.out.print("Nome de utilizador: ");
                System.out.flush();
                String username = stdin.readLine();
                auth.setUsername(username);

                System.out.print("Palavra-passe: ");
                System.out.flush();
                auth.setPassword(stdin.readLine());

                // Try to authenticate
                Authentication authToSend = auth.build();
                byte[] size = ByteBuffer.allocate(4).putInt(authToSend.getSerializedSize()).array();
                this.os.write(size);
                authToSend.writeTo(os);
                this.os.flush();

                // Receive authentication confirmation
                byte[] header = this.is.readNBytes(4);
                int size1 = ByteBuffer.wrap(header).getInt();
                byte[] data = this.is.readNBytes(size1);

                ServerResponse response = ServerResponse.parseFrom(data);
                if (!response.getSuccess()) {
                    if (method.equals("l"))
                        System.out.println("O nome de utilizador não existe ou a palavra passe está incorreta.");
                    else
                        System.out.println("O nome de utilizador já existe.");
                    continue;
                }

                System.out.println("Autenticação bem sucedida.");

                this.username = username;

                break;
            }

            waitConfirmation();

            this.catalog = new Socket("localhost", 9999);
            //this.subscriptions = new Socket("localhost", 9999);
            
            PrintWriter outputTerminal = new PrintWriter(new File("/proc/" + outputPID + "/fd/1"));
            
            // Iniciar thread para ler do socket para o terminal
            Thread stc = new SocketToClient(is, outputTerminal);
            stc.start();
            if (role.equals("f"))
                producerMenu();
            else {
                // Iniciar thread para ler das subscrições para o terminal
                this.subscriptions = ctx.createSocket(ZMQ.SUB);
                this.subscriptions.connect("tcp://localhost:8888");
                Thread subToC = new SubscriptionsToClient(this.subscriptions, outputTerminal);
                subToC.start();

                importerMenu();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class SocketToClient extends Thread {
    InputStream is;
    PrintWriter outputTerminal;

    public SocketToClient(InputStream is, PrintWriter outputTerminal) {
        this.is = is;
        this.outputTerminal = outputTerminal;
    }

    public void run() {
        try {
            while (true) {
                byte[] header = this.is.readNBytes(4);
                int size = ByteBuffer.wrap(header).getInt();
                byte[] data = this.is.readNBytes(size);

                Response response = Response.parseFrom(data);
                switch (response.getResCase()) {
                case IMPORTER: {
                    ImporterResponse ir = response.getImporter();
                    String producerName = ir.getProducerName();
                    String productName = ir.getProductName();
                    int quantity = (int)ir.getQuantity();
                    int price = (int) ir.getPrice();

                    outputTerminal.println("Encomenda efetuada:");
                    outputTerminal.println("\tProduto: " + quantity + " unidades de " + productName);
                    outputTerminal.println("\tPreço por unidade: " + price + "€");
                    outputTerminal.println("\tFabricante: " + producerName);

                    break;
                }

                case PRODUCER: {
                    DealerTimeout dt = response.getProducer();
                    String productName = dt.getProductName();
                    if (dt.getSuccess()) {
                        outputTerminal.println("Venda efetuada:");
                        outputTerminal.println("\tProduto: " + productName);
                        outputTerminal.println("\tCompradores:");
                        for (SaleInfo si : dt.getSalesList()) {
                            outputTerminal.println("\t\t" + si.getUsername() + ": " + si.getQuantity() + " unidades a "
                                    + si.getPrice() + "€/unidade");
                        }
                    } else {
                        outputTerminal.println("Venda de " + productName  + " recusada");
                    }

                    break;
                }

                default:
                    break;
                }

                outputTerminal.println();
                outputTerminal.println("-----------------");
                outputTerminal.println();

                outputTerminal.flush();
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-1);
        }
    }
}

class SubscriptionsToClient extends Thread {
    org.zeromq.ZMQ.Socket subscriptions;
    PrintWriter outputTerminal;
    ZMsg message = new ZMsg();

    public SubscriptionsToClient(org.zeromq.ZMQ.Socket subscriptions, PrintWriter outputTerminal) {
        this.subscriptions = subscriptions;
        this.outputTerminal = outputTerminal;
    }

    public void run() {
        try {
            while (true) {
                message = ZMsg.recvMsg(this.subscriptions);

                outputTerminal.print("Nova oferta disponível para o par: ");
                outputTerminal.println(new String(message.popString()));

                outputTerminal.flush();
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-1);
        }
    }
}
