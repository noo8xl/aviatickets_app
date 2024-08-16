package aviatickets.app.util;

import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

@RequiredArgsConstructor
@Component
public class NotificationServerStarter implements CommandLineRunner {

private final Logger log = LoggerFactory.getLogger(NotificationServerStarter.class);


	/*
	** this implementation is ONLY for test api
	** any production usage is not recommended
	 */
	@Override
	public void run(String... args) throws Exception {
		try {
			// Using ProcessBuilder to execute a shell command
			ProcessBuilder processBuilder = new ProcessBuilder();

			// Command to run (e.g., "ls -l" for listing files in a directory)
//			log.info("Starting notification server---------------------------------------------------->>");
//			processBuilder.command("ls");
			processBuilder.command("./notification-server");
			Process process = processBuilder.start();

			// Reading the output
			BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
			String line;
			while ((line = reader.readLine()) != null) {
				System.out.println(line);
			}

			int exitCode = process.waitFor();
			System.out.println("\nExited with code : " + exitCode);

		} catch (Exception e) {
			e.printStackTrace();
		}
	};

}
