package aviatickets.app.notification;

import aviatickets.app.notification.dto.NewNotifDto;
import aviatickets.app.notification.entity.Notification;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpRequest;

@NoArgsConstructor
@Service
public class NotificationService implements NotificationInterface{


	@Value("${spring.datasource.notification-api-path}")
	private String apiPath;

	@Value("${spring.datasource.notification-api-access-key}")
	private String apiAccessKey;

	@Value("${spring.datasource.avia-tickets-app-api-path}")
	private String myApi;

	private final Notification notification = new Notification();


	@Override
	public void sendNewPwd(NewNotifDto dto) throws URISyntaxException {
		String msg = String.format("Your new password is %s", dto.data());
		this.setParamsAndPerformRequest(dto, msg);
	}

	@Override
	public void sendTwoStepCode(NewNotifDto dto) throws URISyntaxException {
		String msg = String.format("Your two step authentication code is %s.", dto.data());
		this.setParamsAndPerformRequest(dto, msg);
	}


	@Override
	public void sendNewPurchaseEmail(String email, Integer purchaseId) {

		String link = String.format("some_client_link_to_the_detailed_page/{%s}/", purchaseId);
		String msg = String.format("Your purchase successfully created. To get more details follow the link %s", link);
		this.notification.setNotification("email", this.myApi, msg, email);

		try {
			this.sendPOSTRequest();
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void sendCustomNotification(NewNotifDto dto) throws URISyntaxException {
		String msg = String.format("Your two step authentication code is %s.", dto.data());
		this.setParamsAndPerformRequest(dto, msg);
	}

	@Override
	public void sendErrorMessage(String message) {
		try {
			this.sendGETRequest(message);
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	// #########################################################################################

	private void setParamsAndPerformRequest(NewNotifDto dto, String msg) throws URISyntaxException {
		switch (dto.type()){
			case "email":
				this.notification.setNotification("email", this.myApi, msg, dto.recipient());
				this.sendPOSTRequest();
			case "telegram":
				this.notification.setNotification("telegram", this.myApi, msg, dto.recipient());
				this.sendPOSTRequest();
			default:
				throw new RuntimeException("unknown notification type");
		}
	}

	private void sendGETRequest(String msg) throws URISyntaxException {

		String uri = String.format("%s/%s", this.apiPath, msg);

		HttpRequest request = HttpRequest.newBuilder()
				.uri(new URI(uri))
				.header("AccessToken", this.apiAccessKey)
				.header("Content-Type", "application/json")
				.GET()
				.build();


	}

	private void sendPOSTRequest () throws URISyntaxException {

		String uri = String.format("%s/send-user-message/", this.apiPath);

		HttpRequest request = HttpRequest.newBuilder()
			.uri(new URI(uri))
			.header("AccessToken", "value1")
			.header("Content-Type", "application/json")
			.POST(HttpRequest.BodyPublishers.ofString(this.notification.toString()))
			.build();

	}
}














