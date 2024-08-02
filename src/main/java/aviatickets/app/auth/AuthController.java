package aviatickets.app.auth;

import java.sql.SQLException;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import jakarta.validation.Valid;


@RestController
@RequestMapping("/auth")
public class AuthController {
  private final AuthService authService;

	public AuthController(AuthService authService) {
		this.authService = authService;
	}

	// signUp -> registration for the new customer
	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/signUp/")
	void signUp(@Valid @RequestBody SignUpDto dto) throws SQLException, ClassNotFoundException {
		this.authService.signUp(dto);
	}

	// signIn -> login area
	@ResponseStatus(HttpStatus.OK)
	@PostMapping("/signIn/")
	ResponseEntity<SignInResponse> signIn(@Valid @RequestBody SignInDto dto) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.authService.signIn(dto));
	}

  // forgotPassword -> send new pwd to user email
  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/forgot-password/{email}/")
  void forgotPassword(@PathVariable String email) throws SQLException, ClassNotFoundException {
		this.authService.forgotPassword(email);
  }

	// check2faStatus -> check 2fa status
	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/check-2fa/{email}/")
	ResponseEntity<Boolean> check2faStatus(@Valid @PathVariable String email) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.authService.checkTwoStepStatus(email));
	}

}
