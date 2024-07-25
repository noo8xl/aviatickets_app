package aviatickets.app.customer.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import org.hibernate.validator.constraints.Length;

public record UpdateCustomerDto(
		@Positive
		Integer id,
		@NotEmpty
		@Length(min = 4, max = 28)
		String name,
		@NotEmpty
		@Email
		String email,
		@NotEmpty
		@Length(min = 8, max = 30)
		String password
) {
}
