package aviatickets.app.actions.entity;

import java.sql.Date;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.Timestamp;

public record ActionLog(
  @Positive
  Integer id,
  @NotEmpty
	@Email
  String email,
	@Timestamp
  Date date,
  @NotEmpty
  String action,
  @Positive
  Integer customerId
  // .... ->  
) {}