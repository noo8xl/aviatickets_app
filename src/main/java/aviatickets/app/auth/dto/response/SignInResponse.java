package aviatickets.app.auth.dto.response;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import org.hibernate.validator.constraints.Length;

public record SignInResponse(
  // Token t,
	@Positive
	Integer customerId,
	@NotEmpty
	@Length(min = 4, max = 28)
	String name,
	@NotEmpty
	@Email
	String email,
	@NotEmpty
	String role,
	Boolean isBanned,
	Boolean twoStepStatus
) {}
