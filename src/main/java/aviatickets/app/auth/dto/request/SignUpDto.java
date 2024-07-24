package aviatickets.app.auth.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import org.hibernate.validator.constraints.Length;

public record SignUpDto(
  @NotEmpty
	@Length(min = 4, max = 28)
  String name,
  @NotEmpty
	@Email
  String email,
  @NotEmpty
	@Length(min = 8, max = 30)
  String password
) {}
