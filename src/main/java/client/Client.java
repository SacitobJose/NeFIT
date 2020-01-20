package client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.atomic.AtomicBoolean;

import protos.Protos.GETProducerInfo;
import protos.Protos.GETProducerInfoResponse;
import protos.Protos.POSTNegotiation;
import protos.Protos.Subscribe;
import protos.Protos.Unsubscribe;
import protos.Protos.CatalogRequest;
import protos.Protos.GETEntities;
import protos.Protos.GETEntitiesResponse;

public class Client {
    public static void main(String[] args) {
        try {
            Socket s = new Socket("localhost", 1234);
            Thread cts = new ClientToSocket(s);
            cts.start();
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao servidor.");
        }
    }
}

class ClientToSocket extends Thread {
    Socket socket;
    BufferedReader is;
    PrintWriter os;
    String username;
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

    AtomicBoolean wantPrintUpdates = new AtomicBoolean(false);
    AtomicBoolean canPrint = new AtomicBoolean(false);

    AtomicBoolean wantPrintSubscriptions = new AtomicBoolean(false);
    AtomicBoolean canPrintSubscriptions = new AtomicBoolean(false);

    Socket catalog;
    Socket subscriptions;

    public ClientToSocket(Socket cli) throws IOException {
        this.socket = cli;
        is = new BufferedReader(new InputStreamReader(cli.getInputStream()));
        os = new PrintWriter(cli.getOutputStream());
        this.username = null;
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
            main.append("7 - Atualizações\n");
            main.append("8 - Atualizações de subscrições\n");
            main.append("9 - Sair\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 9);

            switch (escolha) {
            case 1:
                StringBuilder transaction = new StringBuilder();
                transaction.append("Import_");

                transaction.append(username);
                transaction.append("_");

                System.out.print("Nome do produto: ");
                System.out.flush();
                transaction.append(this.stdin.readLine());

                transaction.append("_");

                System.out.print("Nome de fabricante: ");
                System.out.flush();
                transaction.append(this.stdin.readLine());

                transaction.append("_");

                System.out.print("Quantidade: ");
                System.out.flush();
                transaction.append(this.readInt());

                transaction.append("_");

                System.out.print("Preço por produto: ");
                System.out.flush();
                transaction.append(this.readInt());

                this.os.println(transaction.toString());
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
                    System.out.println("\tEntidades:");
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
                    System.out.println("\tEntidades:");
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

                Subscribe.Builder sub = Subscribe.newBuilder();
                sub.setUsername(username);
                sub.setProducerName(producer1);
                sub.setProductName(product1);

                CatalogRequest.Builder crsub = CatalogRequest.newBuilder();
                crsub.setSub(sub);
                crsub.build().writeDelimitedTo(this.subscriptions.getOutputStream());
                break;
            case 6:
                System.out.print("Fabricante: ");
                System.out.flush();

                String producer2 = this.stdin.readLine();

                System.out.print("Produto: ");
                System.out.flush();

                String product2 = this.stdin.readLine();

                Unsubscribe.Builder unsub = Unsubscribe.newBuilder();
                unsub.setUsername(username);
                unsub.setProducerName(producer2);
                unsub.setProductName(product2);

                CatalogRequest.Builder crunsub = CatalogRequest.newBuilder();
                crunsub.setUnsub(unsub);
                crunsub.build().writeDelimitedTo(this.subscriptions.getOutputStream());
                break;
            case 7:
                this.canPrint.set(true);
                synchronized (canPrint) {
                    this.canPrint.notify();
                }
                synchronized (wantPrintUpdates) {
                    while (wantPrintUpdates.get())
                        wantPrintUpdates.wait();
                }
                this.canPrint.set(false);
                break;
            case 8:
                this.canPrintSubscriptions.set(true);
                synchronized (canPrintSubscriptions) {
                    this.canPrintSubscriptions.notify();
                }
                synchronized (wantPrintSubscriptions) {
                    while (wantPrintSubscriptions.get())
                        wantPrintSubscriptions.wait();
                }
                this.canPrintSubscriptions.set(false);
                break;
            case 9:
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
            main.append("4 - Atualizações\n");
            main.append("5 - Sair\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 5);

            switch (escolha) {
            case 1:
                StringBuilder transaction = new StringBuilder();
                transaction.append("Produce_");

                transaction.append(username);
                transaction.append("_");

                System.out.print("Nome do produto: ");
                System.out.flush();
                String productName = this.stdin.readLine();
                transaction.append(productName);

                transaction.append("_");

                System.out.print("Quantidade mínima: ");
                System.out.flush();
                transaction.append(this.readInt());

                transaction.append("_");

                System.out.print("Quantidade máxima: ");
                System.out.flush();
                transaction.append(this.readInt());

                transaction.append("_");

                System.out.print("Preço mínimo por produto: ");
                System.out.flush();
                transaction.append(this.readInt());

                transaction.append("_");

                System.out.print("Período de negociação (segundos): ");
                System.out.flush();
                transaction.append(this.readInt());

                this.os.println(transaction);
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
                this.canPrint.set(true);
                synchronized (canPrint) {
                    this.canPrint.notify();
                }
                synchronized (wantPrintUpdates) {
                    while (wantPrintUpdates.get())
                        wantPrintUpdates.wait();
                }
                this.canPrint.set(false);
                break;
            case 5:
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
                StringBuilder auth = new StringBuilder();
                auth.append("Authentication_");

                System.out.print("Deseja fazer (l)ogin ou (r)egistar-se? ");
                System.out.flush();
                String method = stdin.readLine();
                if (method.equals("r"))
                    auth.append(1);
                else if (method.equals("l"))
                    auth.append(0);
                else {
                    System.out.println("Não é nenhum dos métodos válidos.");
                    continue;
                }

                auth.append("_");

                System.out.print("É um (f)abricante ou um (i)mportador? ");
                System.out.flush();
                role = stdin.readLine();
                if (role.equals("f"))
                    auth.append(0);
                else if (role.equals("i"))
                    auth.append(1);
                else {
                    System.out.println("Não é nenhum dos papéis válidos.");
                    continue;
                }

                auth.append("_");

                System.out.print("Nome de utilizador: ");
                System.out.flush();
                String username = stdin.readLine();
                auth.append(username);

                auth.append("_");

                System.out.print("Palavra-passe: ");
                System.out.flush();
                auth.append(stdin.readLine());

                // Try to authenticate
                this.os.println(auth.toString());
                this.os.flush();

                // Receive authentication confirmation
                String response = this.is.readLine();
                String[] res = response.split("_");
                if (res[1].equals("0")) {
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
            this.subscriptions = new Socket("localhost", 9999);

            // Iniciar thread para ler do socket para o terminal
            Thread stc = new SocketToClient(is, wantPrintUpdates, canPrint, stdin);
            stc.start();
            if (role.equals("f"))
                producerMenu();
            else {
                // Iniciar thread para ler das subscrições para o terminal
                Thread subToC = new SubscriptionsToClient(this.subscriptions.getInputStream(), wantPrintSubscriptions,
                        canPrintSubscriptions, stdin);
                subToC.start();

                importerMenu();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class SocketToClient extends Thread {
    BufferedReader is;
    AtomicBoolean canPrint;
    AtomicBoolean wantPrintUpdates;
    BufferedReader stdin;

    public SocketToClient(BufferedReader is, AtomicBoolean wantPrintUpdates, AtomicBoolean canPrint,
            BufferedReader stdin) throws IOException {
        this.is = is;
        this.canPrint = canPrint;
        this.wantPrintUpdates = wantPrintUpdates;
        this.stdin = stdin;
    }

    private void waitConfirmation() throws IOException {
        System.out.print("\nClique no ENTER para continuar...");
        System.out.flush();
        this.stdin.readLine();
    }

    public void run() {
        try {
            while (true) {
                String line = this.is.readLine();

                this.wantPrintUpdates.set(true);
                synchronized (this.canPrint) {
                    while (!this.canPrint.get())
                        this.canPrint.wait();
                }

                String[] parts = line.split("_");
                if (parts[0].equals("Importer")) {
                    String producerName = parts[1];
                    String productName = parts[2];
                    int quantity = Integer.parseInt(parts[3]);
                    int price = Integer.parseInt(parts[4]);

                    System.out.println("Encomenda efetuada:");
                    System.out.println("\tProduto: " + quantity + " unidades de " + productName);
                    System.out.println("\tPreço por unidade: " + price + "€");
                    System.out.println("\tFabricante: " + producerName);
                } else {
                    Boolean success = Boolean.parseBoolean(parts[2]);
                    if (success) {
                        String productName = parts[4];
                        System.out.println("Venda efetuada:");
                        System.out.println("\tProduto: " + productName);
                        System.out.println("\tCompradores:");
                        for (int i = 5; i < parts.length; i += 3) {
                            System.out.println("\t\t" + parts[i] + ": " + parts[i + 1] + " unidades a " + parts[i + 2]
                                    + "€/unidade");
                        }
                    } else {
                        System.out.println("Venda de " + parts[4] + " recusada");
                    }
                }

                this.waitConfirmation();
                synchronized (wantPrintUpdates) {
                    this.wantPrintUpdates.set(false);
                    this.wantPrintUpdates.notify();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-1);
        }
    }
}

class SubscriptionsToClient extends Thread {
    InputStream is;
    AtomicBoolean canPrintSubscriptions;
    AtomicBoolean wantPrintSubscriptions;
    BufferedReader stdin;

    public SubscriptionsToClient(InputStream is, AtomicBoolean wantPrintSubscriptions,
            AtomicBoolean canPrintSubscriptions, BufferedReader stdin) throws IOException {
        this.is = is;
        this.canPrintSubscriptions = canPrintSubscriptions;
        this.wantPrintSubscriptions = wantPrintSubscriptions;
        this.stdin = stdin;
    }

    private void waitConfirmation() throws IOException {
        System.out.print("\nClique no ENTER para continuar...");
        System.out.flush();
        this.stdin.readLine();
    }

    public void run() {
        try {
            while (true) {
                POSTNegotiation pn = POSTNegotiation.parseDelimitedFrom(this.is);

                this.wantPrintSubscriptions.set(true);
                synchronized (this.canPrintSubscriptions) {
                    while (!this.canPrintSubscriptions.get())
                        this.canPrintSubscriptions.wait();
                }

                System.out.println("Produtor: " + pn.getProducerName());
                System.out.println("Produto: " + pn.getProductName());
                System.out.println("Quantidade máxima: " + pn.getMaximumAmount());
                System.out.println("Quantidade mínima: " + pn.getMinimumAmount());
                System.out.println("Preço mínimo: " + pn.getMinimumUnitaryPrice());
                System.out.println("Período de negociação: " + pn.getNegotiationPeriod());

                this.waitConfirmation();
                synchronized (wantPrintSubscriptions) {
                    this.wantPrintSubscriptions.set(false);
                    this.wantPrintSubscriptions.notify();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-1);
        }
    }
}
