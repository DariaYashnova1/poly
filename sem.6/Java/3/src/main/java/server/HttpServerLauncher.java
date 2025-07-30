package server;

import server.handlers.*;

import java.io.IOException;

public class HttpServerLauncher {
    public static void main(String[] args) throws IOException {
        boolean useVirtualThreads = true;
        boolean useGson = true;

        HttpServer server = new HttpServer("0.0.0.0", 8081, 100, useVirtualThreads);

        // Регистрация обработчиков
        if (useGson) {
            server.addHandler("POST", "/request1", Request1Handler::handleWithGson);
            server.addHandler("POST", "/request2", Request2Handler::handleWithGson);
        } else {
            server.addHandler("POST", "/request1", Request1Handler::handleWithCustomParser);
            server.addHandler("POST", "/request2", Request2Handler::handleWithCustomParser);
        }

        server.start();
    }
}