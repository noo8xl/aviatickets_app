package aviatickets.app.customer.entity;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

import java.sql.Date;

public record Customer(
		@Positive Integer id,
		@NotEmpty String name,
		@NotEmpty String email,
		@NotEmpty String password,
		Date createdAt,
		@NotEmpty String role,
		Boolean isBanned,
		Boolean twoStepStatus
) {

}
