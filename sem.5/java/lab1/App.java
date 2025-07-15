package ru.spbstu.telematics.java;
import java.util.HashMap;
import java.util.Random;

public class App{

    public static void main(String[] args) {
        int numValues = 100;
        HashMap<Double,Integer> mp = new HashMap<>();
        double mean = 0.0;
        double standardDeviation = 1.0;

      
        Random random = new Random();

        for (int i = 0; i < numValues; i++) {
            double randomValue = generateNormalDistribution(random, mean, standardDeviation);
            System.out.println(randomValue);
        }
    }

    // Функция для генерации случайной величины с нормальным распределением
    private static double generateNormalDistribution(Random random, double mean, double standardDeviation) {

        double u0 = (random.nextDouble()-0.5)*2;
        double u1 =(random.nextDouble()-0.5)*2;
        double s = Math.pow(u1, 2) + Math.pow(u0, 2);
        while (s > 1 || s == 0) {
            u0 =(random.nextDouble()-0.5)*2;
            u1 = (random.nextDouble()-0.5)*2;
            s = Math.pow(u1, 2) + Math.pow(u0, 2);
        }
        double z0 = u0 * Math.sqrt(-2 * Math.log(s) / s);
        double z1 = u1 * Math.sqrt(-2 * Math.log(s) / s);
        boolean b = random.nextBoolean();


        if (b) return (mean + standardDeviation * z0);
        else return (mean + standardDeviation * z1);
    }
}