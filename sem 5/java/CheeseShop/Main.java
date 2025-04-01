import java.lang.Thread;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Main {
    public static void main(String[] args) throws InterruptedException {
        Random random = new Random();
        CheeseShop shop = new CheeseShop(9);
        Thread shopThread = new Thread(shop);
        for (int i = 0; i < 10; i++) {
            Buyer buyer;
            if (random.nextBoolean()){
                buyer = new HumbleBuyer(shop,"Humble " + i);
            }else{
                buyer = new BraveBuyer(shop,"Brave " + i);
            }
            buyer.start();
        }
        //Thread.sleep(2000);

        shop.printQueue();

        shopThread.start();
        //Thread.sleep(5000);
        Buyer nbuyer = new BraveBuyer(shop,"Brave #"  );
        nbuyer.start();
        //Thread.sleep(5000);
        Buyer nbuyer2 = new HumbleBuyer(shop,"Humble *"  );
        nbuyer2.start();


    }
}