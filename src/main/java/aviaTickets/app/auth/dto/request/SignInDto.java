package aviaTickets.app.auth.dto.request;

import jakarta.validation.constraints.NotEmpty;

public record SignInDto(
  @NotEmpty
  String email,
  @NotEmpty
  String password,
  Boolean twoStep,
  String code
) {}
