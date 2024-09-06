package aviatickets.app.auth.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import org.hibernate.validator.constraints.Length;

public record SignInDto(
  @NotEmpty
	@Email
  String email,
  @NotEmpty
	@Length(min = 8, max = 30)
  String password,
	@NotEmpty
  Boolean twoStep,
  String code
) {}
