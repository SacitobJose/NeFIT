package catalog;

import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Proxy extends Thread{
    private ZMQ.Poller items;
    private ZMQ.Socket s1, s2;

    Proxy(ZContext context, ZMQ.Socket s1, ZMQ.Socket s2) {
        this.s1 = s1;
        this.s2 = s2;
        items = context.createPoller(2);
        items.register(s1, ZMQ.Poller.POLLIN);
        items.register(s2, ZMQ.Poller.POLLIN);
    }

    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            items.poll();
            ZMQ.Socket from, to;
            if (items.pollin(0)) {
                from = s1;
                to = s2;
                System.out.println("From s1");
            } else {
                from = s2;
                to = s1;
                System.out.println("From s2");
            }
            while (true) {
                byte[] m = from.recv();
                System.out.println(m[0]);
                System.out.println(new String(m));
                if (from.hasReceiveMore()) {
                    to.sendMore(m);
                } else {
                    to.send(m);
                    break;
                }
            }
        }
    }
}

/* Client read
    while (true) {
        byte[] b = this.subSocket.recv();

        synchronized (this.wantWriteSubscriptions) {
            this.wantWriteSubscriptions.set(true);
        }
        synchronized (this.canWrite) {
            while (!this.canWrite.get()) {
                this.canWrite.wait();
            }
        }

        System.out.println(new String(b));

        this.waitConfirmation();
    }
*/

/* Client subscribe
    ZContext context = new ZContext();
    ZMQ.Socket subSocket = context.createSocket(SocketType.SUB);
    subSocket.connect("tcp://localhost:7777");

    ...

    subSocket.subscribe("producer");
    
    ...

    subSocket.unsubscribe("producer");
*/

/* Dealer publish
    ZContext context = new ZContext();
    ZMQ.Socket pubSocket = context.createSocket(SocketType.PUB);
    pubSocket.connect("tcp://localhost:8888");

    pubSocket.send("...");
*/
