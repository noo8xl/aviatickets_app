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
	private String content; // content to send as a message
	@NotEmpty
	private String recipient;  // user email or telegram chatId


	public void setNotification(String serviceType, String domainName, String content, String recipient) {
			this.serviceType = serviceType;
			this.domainName = domainName;
			this.content = content;
			this.recipient = recipient;
	}

}
