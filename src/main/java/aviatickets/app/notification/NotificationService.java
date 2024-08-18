package aviatickets.app.notification;

import aviatickets.app.notification.dto.NewNotifDto;
import aviatickets.app.notification.entity.Notification;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.*;
//import java.net.http.HttpClient;
import java.net.http.*;
import java.net.http.HttpResponse.BodyHandler;
import java.nio.charset.StandardCharsets;

@NoArgsConstructor
@Service
public class NotificationService implements NotificationInterface {

	@Value("${spring.datasource.notification-api-path}")
	private String apiPath;

	@Value("${spring.datasource.notification-api-access-key}")
	private String apiAccessKey;

	private final String myApiName = "localhost.domain.test";

	private final Notification notification = new Notification();


	@Override
	public void sendNewPwd(NewNotifDto dto) {
		String msg = String.format("Your new password is %s", dto.data());
		this.setParamsAndPerformRequest(dto, msg);
	}

	@Override
	public void sendTwoStepCode(NewNotifDto dto) {
		String msg = String.format("Your two step authentication code is %s.", dto.data());
		this.setParamsAndPerformRequest(dto, msg);
	}

	@Override
	public void sendRegistrationEmail(String email) {

	}


	@Override
	public void sendNewPurchaseEmail(String email, Integer purchaseId) {
		String link = String.format("some_client_link_to_the_detailed_page/{%s}/", purchaseId);
		String msg = String.format("Your purchase successfully created. To get more details follow the link %s", link);
		this.notification.setNotification("email", this.myApiName, email, msg);
		this.sendPOSTRequest();
	}

	@Override
	public void sendCustomNotification(NewNotifDto dto) {
		String msg = String.format("Your two step authentication code is %s.", dto.data());
		this.setParamsAndPerformRequest(dto, msg);
	}

	@Override
	public void sendErrorMessage(String message) {
		this.sendGETRequest(message);
	}

	// #########################################################################################

	private void setParamsAndPerformRequest(NewNotifDto dto, String msg) {
		switch (dto.type()){
			case "email":
				this.notification.setNotification("email", this.myApiName, dto.recipient(), msg);
				this.sendPOSTRequest();
			case "telegram":
				this.notification.setNotification("telegram", this.myApiName, dto.recipient(), msg);
				this.sendPOSTRequest();
			default:
				throw new RuntimeException("unknown notification type");
		}
	}

	private void sendGETRequest(String msg) {

		String uri = String.format("%s/%s", this.apiPath, msg);

		try {
			// Define the URL for the request
			URL url = new URI(uri).toURL();

			HttpURLConnection connection = (HttpURLConnection) url.openConnection();;

			// Open a connection to the URL
			connection = (HttpURLConnection) url.openConnection();

			// Set the request method to GET
			connection.setRequestMethod("GET");
			connection.setRequestProperty("AccessToken", this.apiAccessKey);
			connection.setRequestProperty("Content-Type", "application/json");

//			connection.connect();

			// Get the response code (e.g., 200 for OK)
			int responseCode = connection.getResponseCode();
			System.out.println("Response Code: " + responseCode);

			// Check if the response is successful (response code 200)
			if (responseCode == HttpURLConnection.HTTP_OK) {
				// Create a BufferedReader to read the response
				BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
				String inputLine;
				StringBuilder response = new StringBuilder();

				// Read the response line by line
				while ((inputLine = reader.readLine()) != null) {
					response.append(inputLine);
				}

				// Close the BufferedReader
				reader.close();

				// Print the full response
				System.out.println("Response: " + response.toString());
			} else {
				System.out.println("GET request failed.");
			}

			// Close the connection
			connection.disconnect();

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void sendPOSTRequest () {

		String uri = String.format("%s/send-user-message/", this.apiPath);

		try {
			// Define the URL for the request
			URL url = new URI(uri).toURL();

			HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			;

			// Set the request method to POST
			connection.setRequestMethod("POST");

			// Set the request headers
			connection.setRequestProperty("Content-Type", "application/json; utf-8");
			connection.setRequestProperty("AccessToken", this.apiAccessKey);
			connection.setRequestProperty("Accept", "application/json");

			// Enable input and output streams for sending data
			connection.setDoOutput(true);

			// JSON data to send in the request body
			String jsonInputString = this.notification.toString();

			System.out.println("jsonInputString: \n->" + jsonInputString);



			// Send the JSON data
			try (OutputStream os = connection.getOutputStream()) {
				byte[] input = jsonInputString.getBytes(StandardCharsets.UTF_8);
				os.write(input, 0, input.length);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}

			// Get the response code
			int responseCode = connection.getResponseCode();
			System.out.println("Response Code: " + responseCode);

			// Read the response if the request was successful
			if (responseCode == HttpURLConnection.HTTP_OK) {
				BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
				String inputLine;
				StringBuilder response = new StringBuilder();

				while ((inputLine = in.readLine()) != null) {
					response.append(inputLine);
				}

				// Close the input stream
				in.close();

				// Print the response
				System.out.println("Response: " + response.toString());
			} else {
				System.out.println("POST request failed.");
			}


		} catch (Exception e) {
			throw new RuntimeException(e);
		}

	}


}














