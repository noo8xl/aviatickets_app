package aviatickets.app.notification.entity;

import jakarta.validation.constraints.NotEmpty;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class Notification {

	@NotEmpty
	private String serviceType;	// email OR telegram
	@NotEmpty
	private String domainName; // name of client API
	@NotEmpty
	private String recipient;  // user email or telegram chatId
	@NotEmpty
	private String content; // content to send as a message


	public void setNotification(String serviceType, String domainName, String recipient, String content) {
		this.serviceType = serviceType;
		this.domainName = domainName;
		this.recipient = recipient;
		this.content = content;
	}

}
