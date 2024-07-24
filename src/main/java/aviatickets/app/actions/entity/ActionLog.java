package aviatickets.app.actions.entity;

import java.sql.Date;
import java.time.LocalDateTime;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record ActionLog(
  @Positive
  Integer id,
  @NotEmpty
  String email,

  Date date,
  @NotEmpty
  String action,
  @Positive
  Integer customerId
  // .... ->  
) {}