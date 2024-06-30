package aviatickets.app.actions.entity;

import java.time.LocalDateTime;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record ActionLog(
  @Positive
  Integer id,
  @NotEmpty
  String email,

  LocalDateTime date,
  @NotEmpty
  String action,
  @Positive
  Integer customerId
  // .... ->  
) {}