package aviatickets.app.auth.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;

public record SignInDto(
  @NotEmpty
	@Email
  String email,
  @NotEmpty
  String password,
  Boolean twoStep,
  String code
) {}
