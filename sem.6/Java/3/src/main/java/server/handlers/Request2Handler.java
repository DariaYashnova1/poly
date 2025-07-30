package server.handlers;

import com.google.gson.Gson;
import parser.SimpleJson;
import server.HttpServer;

import java.util.*;
import java.util.stream.Collectors;

public class Request2Handler {
    private static final int MAX_VALUES = 1000; // Лимит на количество обрабатываемых значений

    public static HttpServer.HttpResponse handleWithCustomParser(HttpServer.HttpRequest request) {
        return processRequest(request, true);
    }

    public static HttpServer.HttpResponse handleWithGson(HttpServer.HttpRequest request) {
        return processRequest(request, false);
    }

    private static HttpServer.HttpResponse processRequest(HttpServer.HttpRequest request, boolean useCustomParser) {
        try {
            //  Проверка тела запроса
            if (request.bodyAsString() == null || request.bodyAsString().isEmpty()) {
                return createErrorResponse(400, "Request body is empty");
            }

            // Парсинг JSON
            Map<String, Object> data;
            try {
                data = useCustomParser ?
                        SimpleJson.parseToMap(request.bodyAsString()) :
                        new Gson().fromJson(request.bodyAsString(), Map.class);
            } catch (Exception e) {
                return createErrorResponse(400, "Invalid JSON format");
            }

            // Извлечение числовых значений
            List<Double> numbers = extractNumbers(data);
            if (numbers.isEmpty()) {
                return createErrorResponse(400, "No numeric values found");
            }
            if (numbers.size() > MAX_VALUES) {
                return createErrorResponse(400, "Too many values (max " + MAX_VALUES + ")");
            }

            //  Выполнение вычислений
            Map<String, Object> stats = calculateStatistics(numbers);

            //  Формирование ответа
            return new HttpServer.HttpResponse()
                    .setStatusCode(200)
                    .setHeader("Content-Type", "application/json")
                    .setBody(useCustomParser ?
                            SimpleJson.toJson(stats) :
                            new Gson().toJson(stats));

        } catch (Exception e) {
            return createErrorResponse(500, "Internal server error");
        }
    }

    private static List<Double> extractNumbers(Map<String, Object> data) {
        return data.values().stream()
                .filter(Number.class::isInstance)
                .map(v -> ((Number) v).doubleValue())
                .collect(Collectors.toList());
    }

    private static Map<String, Object> calculateStatistics(List<Double> numbers) {
        Collections.sort(numbers);

        // Основные статистические показатели
        double sum = numbers.stream().mapToDouble(Double::doubleValue).sum();
        double average = sum / numbers.size();
        double median = calculateMedian(numbers);
        double stdDev = calculateStandardDeviation(numbers, average);
        List<Double> modes = calculateModes(numbers);
        List<Double> filteredOutliers = filterOutliers(numbers, average, stdDev);

        // Дополнительные вычисления
        double range = numbers.get(numbers.size() - 1) - numbers.get(0);
        double iqr = calculateIQR(numbers);

        return Map.of(
                "count", numbers.size(),
                "sum", sum,
                "average", average,
                "median", median,
                "stdDev", stdDev,
                "range", range,
                "iqr", iqr,
                "modes", modes,
                "filteredOutliers", filteredOutliers,
                "status", "success"
        );
    }

    private static double calculateMedian(List<Double> numbers) {
        int size = numbers.size();
        if (size % 2 == 0) {
            return (numbers.get(size/2 - 1) + numbers.get(size/2)) / 2.0;
        } else {
            return numbers.get(size/2);
        }
    }

    private static double calculateStandardDeviation(List<Double> numbers, double average) {
        double variance = numbers.stream()
                .mapToDouble(n -> Math.pow(n - average, 2))
                .average()
                .orElse(0.0);
        return Math.sqrt(variance);
    }

    private static List<Double> calculateModes(List<Double> numbers) {
        Map<Double, Integer> frequencyMap = new HashMap<>();
        numbers.forEach(n -> frequencyMap.put(n, frequencyMap.getOrDefault(n, 0) + 1));

        int maxFrequency = frequencyMap.values().stream().max(Integer::compare).orElse(0);
        return frequencyMap.entrySet().stream()
                .filter(e -> e.getValue() == maxFrequency)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    private static List<Double> filterOutliers(List<Double> numbers, double average, double stdDev) {
        double lowerBound = average - 2 * stdDev;
        double upperBound = average + 2 * stdDev;
        return numbers.stream()
                .filter(n -> n >= lowerBound && n <= upperBound)
                .collect(Collectors.toList());
    }

    private static double calculateIQR(List<Double> numbers) {
        int q1Index = numbers.size() / 4;
        int q3Index = numbers.size() * 3 / 4;
        return numbers.get(q3Index) - numbers.get(q1Index);
    }

    private static HttpServer.HttpResponse createErrorResponse(int statusCode, String message) {
        return new HttpServer.HttpResponse()
                .setStatusCode(statusCode)
                .setHeader("Content-Type", "application/json")
                .setBody("{\"error\":\"" + message + "\"}");
    }
}