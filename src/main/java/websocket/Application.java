package websocket;

import main.GUI;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {

    public static void main(String[] args) {

        Runnable gui = new Runnable() {
            @Override
            public void run() {
                GUI.main(args);
            }
        };
        Thread t =new Thread(gui);
        t.start();

        SpringApplication.run(Application.class, args);

    }
}