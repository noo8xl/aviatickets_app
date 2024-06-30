package aviatickets.app.jwt.entity;

public record Token(
  String accessToken,
  String refreshToken
) {}
