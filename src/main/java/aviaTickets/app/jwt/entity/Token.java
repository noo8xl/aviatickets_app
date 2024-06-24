package aviaTickets.app.jwt.entity;

public record Token(
  String accessToken,
  String refreshToken
) {}
