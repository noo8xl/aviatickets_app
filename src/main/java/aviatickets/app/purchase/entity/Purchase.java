package aviatickets.app.purchase.entity;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

import java.sql.Date;

public record Purchase(
	@Positive
	Integer id,
	@Positive
	@NotEmpty
	Integer flightId,
	@Positive
	@NotEmpty
	Integer customerId,
	Date date
	) {}
