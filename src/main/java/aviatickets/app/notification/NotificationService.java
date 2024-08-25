package aviatickets.app.notification;

import aviatickets.app.notification.dto.NewNotifDto;
import aviatickets.app.notification.dto.NewPurchaseDto;
import aviatickets.app.notification.entity.Notification;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.OutputStream;
import java.net.*;
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
	public void sendNewPurchaseEmail(NewPurchaseDto dto) {
		String link = String.format("some_client_link_to_the_detailed_page/{%s}/", dto.purchaseId());
		String msg = String.format("Your purchase successfully created. To get more details follow the link %s", link);
		this.notification.setNotification("email", this.myApiName, dto.email(), msg);
		this.sendPOSTRequest();
	}

	@Override
	public void sendCustomNotification(NewNotifDto dto) {
		this.setParamsAndPerformRequest(dto, dto.data());
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

		int responseCode = 0;
		URL url = null;
		HttpURLConnection connection = null;
		ObjectMapper mapper = new ObjectMapper();

		String uri = String.format("%s/%s", this.apiPath, msg);

		try {

			url = new URI(uri).toURL();
			connection = (HttpURLConnection) url.openConnection();

			connection.setRequestMethod("GET");
			connection.setRequestProperty("AccessToken", this.apiAccessKey);
			connection.setRequestProperty("Content-Type", "application/json");

			responseCode = connection.getResponseCode();
			System.out.println("Response Code: " + responseCode);

		} catch (Exception e) {
			throw new RuntimeException(e);
		} finally {
			connection.disconnect();
		}
	}

	private void sendPOSTRequest () {

		int responseCode = 0;
		URL url = null;
		HttpURLConnection connection = null;
		ObjectMapper mapper = new ObjectMapper();

		String uri = String.format("%s/send-user-message/", this.apiPath);

		try {

			url = new URI(uri).toURL();
			connection = (HttpURLConnection) url.openConnection();
			String jsonStr = mapper.writeValueAsString(this.notification);

			connection.setRequestMethod("POST");
			connection.setRequestProperty("Content-Type", "application/json; utf-8");
			connection.setRequestProperty("AccessToken", this.apiAccessKey);
			connection.setDoOutput(true);

			System.out.println("jsonInputString: \n->" + jsonStr);
			try (OutputStream os = connection.getOutputStream()) {
				byte[] input = jsonStr.getBytes(StandardCharsets.UTF_8);
				os.write(input, 0, input.length);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}

			responseCode = connection.getResponseCode();
			System.out.println("Response Code: " + responseCode);

		} catch (Exception e) {
			throw new RuntimeException(e);
		} finally {
			connection.disconnect();
		}

	}


}














