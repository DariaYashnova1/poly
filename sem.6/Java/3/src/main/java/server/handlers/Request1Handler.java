package server.handlers;

import com.google.gson.Gson;
import db.DatabaseService;
import parser.SimpleJson;
import server.HttpServer;

import java.util.Map;

public class Request1Handler {
    private static final DatabaseService dbService = new DatabaseService();

    public static HttpServer.HttpResponse handleWithCustomParser(HttpServer.HttpRequest request) {
        try {
            // Парсинг JSON с помощью собственного парсера
            Map<String, Object> data = SimpleJson.parseToMap(request.bodyAsString());

            // Сохранение в SQLite
            dbService.save(data);

            // Получение данных из базы
            Object result = dbService.retrieve();

            // Возврат успешного ответа
            return new HttpServer.HttpResponse()
                    .setStatusCode(200)
                    .setHeader("Content-Type", "application/json")
                    .setBody(SimpleJson.toJson(result));

        } catch (Exception e) {
            // Обработка ошибок
            return new HttpServer.HttpResponse()
                    .setStatusCode(500)
                    .setBody("{\"error\":\"Internal Server Error: " + e.getMessage() + "\"}");
        }
    }

    public static HttpServer.HttpResponse handleWithGson(HttpServer.HttpRequest request) {
        try {
            Gson gson = new Gson();

            // Парсинг JSON с помощью Gson
            Map<String, Object> data = gson.fromJson(request.bodyAsString(), Map.class);

            // Сохранение в SQLite
            dbService.save(data);

            // Получение данных из базы
            Object result = dbService.retrieve();

            // Возврат успешного ответа
            return new HttpServer.HttpResponse()
                    .setStatusCode(200)
                    .setHeader("Content-Type", "application/json")
                    .setBody(gson.toJson(result));

        } catch (Exception e) {
            // Обработка ошибок
            return new HttpServer.HttpResponse()
                    .setStatusCode(500)
                    .setBody("{\"error\":\"Internal Server Error: " + e.getMessage() + "\"}");
        }
    }
}