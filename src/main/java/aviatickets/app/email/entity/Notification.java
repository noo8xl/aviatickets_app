package aviatickets.app.email.entity;

public record Notification(
		String customerEmail,
		String title,
		String body
) {
}
