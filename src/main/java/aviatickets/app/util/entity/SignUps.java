package aviatickets.app.util.entity;

import aviatickets.app.auth.dto.request.SignUpDto;

import java.util.List;

public record SignUps(List<SignUpDto> signUps) {
}
