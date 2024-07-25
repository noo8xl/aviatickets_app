package aviatickets.app.customer.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import org.hibernate.validator.constraints.Length;

public record ChangePwdDto(
  @NotEmpty
	@Email
  String email,
  @NotEmpty
	@Length(min = 8, max = 30)
  String pwd
) {}
