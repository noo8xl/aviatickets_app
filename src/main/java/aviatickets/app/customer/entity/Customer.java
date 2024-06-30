package aviatickets.app.customer.entity;

import java.util.Date;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record Customer(
		@Positive Integer id,
		@NotEmpty String name,
		@NotEmpty String email,
		@NotEmpty String password,
		Date createdAt,
		Date updatedAt,
		Boolean isBanned,
		Role role) {

}
