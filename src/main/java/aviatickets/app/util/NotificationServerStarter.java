package aviatickets.app.util;

import lombok.NoArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.io.BufferedReader;
import java.io.InputStreamReader;

@NoArgsConstructor
@Component
public class NotificationServerStarter implements CommandLineRunner {

	private final Logger log = LoggerFactory.getLogger(NotificationServerStarter.class);

	/*
	** this implementation is ONLY for test api
	** to see how it works.
	 */
	@Override
	public void run(String... args){
		try {
			String line;
			int exitCode;
			ProcessBuilder processBuilder = new ProcessBuilder();

			processBuilder.command("./notification-server");
			Process process = processBuilder.start();

			BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));

			while ((line = reader.readLine()) != null) {
				log.info("Notification server -> {}",line);
			}

			exitCode = process.waitFor();
			log.info("\nExited with code : {}", exitCode);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	};

}
