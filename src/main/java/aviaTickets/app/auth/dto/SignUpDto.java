package aviaTickets.app.auth.dto;

public record SignUpDto(
  String name,
  String email,
  String password
) {}
