package aviatickets.app;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.BufferedImageHttpMessageConverter;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.springframework.http.converter.HttpMessageConverter;

@SpringBootApplication
@EnableCaching
public class Application {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
		System.out.println("Server is running...");
	}

	@Bean
	public HttpMessageConverter<BufferedImage> createImageHttpMessageConverter() {
		return new BufferedImageHttpMessageConverter();
	}

//	@Bean
//	public CommandLineRunner commandLineRunner() {
//		return _ -> {
//			try {
//				// Using ProcessBuilder to execute a shell command
//				ProcessBuilder processBuilder = new ProcessBuilder();
//
//				// Command to run (e.g., "ls -l" for listing files in a directory)
//				processBuilder.command("/home/noo8xl/Documents/work_space/aviatickets_app/src/main/resources/notification-server");
//				Process process = processBuilder.start();
//
//				// Reading the output
//				BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
//				String line;
//				while ((line = reader.readLine()) != null) {
//					System.out.println(line);
//				}
//
//				int exitCode = process.waitFor();
//				System.out.println("\nExited with code : " + exitCode);
//
//			} catch (Exception e) {
//				e.printStackTrace();
//			}
//		};
//	}

}
