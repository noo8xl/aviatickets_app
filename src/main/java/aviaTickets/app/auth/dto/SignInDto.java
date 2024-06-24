package aviaTickets.app.auth.dto;

public record SignInDto(
  String email,
  String password,
  Boolean twoStep,
  String code
) {}
