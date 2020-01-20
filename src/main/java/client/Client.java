package client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.atomic.AtomicBoolean;

import protos.Protos.GETProducerInfo;
import protos.Protos.GETProducerInfoResponse;
import protos.Protos.POSTNegotiation;
import protos.Protos.CatalogRequest;

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
            main.append("2 - Obter informações\n");
            main.append("3 - Atualizações\n");
            main.append("4 - Sair\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 4);

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

                System.out.print("Nome de produtor: ");
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
                gpi.setProducerName(producer);
                CatalogRequest.Builder cr = CatalogRequest.newBuilder();
                cr.setGpi(gpi);

                Socket catalog = new Socket("localhost", 9999);
                cr.build().writeDelimitedTo(catalog.getOutputStream());

                GETProducerInfoResponse gpir = GETProducerInfoResponse.parseDelimitedFrom(catalog.getInputStream());
                for (POSTNegotiation pn : gpir.getNegotiationsList()) {
                    System.out.println("Produto: " + pn.getProductName());
                    System.out.println("Quantidade máxima: " + pn.getMaximumAmount());
                    System.out.println("Quantidade mínima: " + pn.getMinimumAmount());
                    System.out.println("Preço mínimo: " + pn.getMinimumUnitaryPrice());
                    System.out.println("Período de negociação: " + pn.getNegotiationPeriod());
                }

                waitConfirmation();
                break;
            case 3:
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
            case 4:
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
            main.append("2 - Atualizações\n");
            main.append("3 - Sair\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 3);

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
            case 3:
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

            // Iniciar thread para ler do socket para o terminal
            Thread stc = new SocketToClient(is, wantPrintUpdates, canPrint, stdin);
            stc.start();
            if (role.equals("f"))
                producerMenu();
            else
                importerMenu();
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
                    System.out.println("\tProdutor: " + producerName);
                } else {
                    Boolean success = Boolean.parseBoolean(parts[2]);
                    if (success) {
                        String productName = parts[4];
                        System.out.println("Venda efetuada:");
                        System.out.println("\tProduto: " + productName);
                        System.out.println("\tCompradores:");
                        for (int i = 5; i < parts.length; i += 3) {
                            System.out.println("\t\t" + parts[i] + ": " + parts[i+1]
                                    + " unidades a " + parts[i+2] + "€/unidade");
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
