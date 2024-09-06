package aviatickets.app.auth.dto.response;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.BooleanFlag;
import org.hibernate.validator.constraints.Length;

public record SignInResponse(
	@Positive
	@NotEmpty
	Integer customerId,
	@NotEmpty
	@Length(min = 4, max = 28)
	String name,
	@NotEmpty
	@Email
	String email,

	@BooleanFlag
	@NotEmpty
	Boolean isBanned,
	@BooleanFlag
	@NotEmpty
	Boolean twoStepStatus,
	@NotEmpty
	String token
) {}
