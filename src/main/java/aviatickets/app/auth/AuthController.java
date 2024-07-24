package aviatickets.app.auth;

import java.sql.SQLException;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.CustomerService;
import aviatickets.app.exception.BadRequestException;
import jakarta.validation.Valid;


@RestController
@RequestMapping("/auth")
public class AuthController {
  private final AuthService authService;
  private final CustomerService customerService;

	public AuthController(AuthService authService, CustomerService customerService) {
		this.authService = authService;
		this.customerService = customerService;
	}

	// signUp -> registration for the new customer
	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/signUp/")
	void signUp(@Valid @RequestBody SignUpDto dto) throws SQLException, ClassNotFoundException {
		authService.signUp(dto);
	}

	// signIn -> login area
	@ResponseStatus(HttpStatus.OK)
	@PostMapping("/signIn/")
	ResponseEntity<SignInResponse> signIn(@Valid @RequestBody SignInDto dto) throws SQLException, ClassNotFoundException {
		Boolean twoStepStatus = this.authService.checkTwoStepStatus(dto.email());
		if(Boolean.TRUE.equals(twoStepStatus)) {
			throw new BadRequestException("Two step authentication is enabled.");
		}
		return ResponseEntity.ok(authService.signIn(dto));
	}

  // forgotPassword -> send new pwd to user email
  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/forgot-password/{email}/")
  void forgotPassword(@PathVariable String email) throws SQLException, ClassNotFoundException {
		authService.forgotPassword(email);
  }


//	// signIn -> check 2fa status
//	@ResponseStatus(HttpStatus.OK)
//	@GetMapping("/check-2fa/{email}")
//	ResponseEntity<Boolean> check(@Valid @PathVariable String email) {
//		try {
//			Boolean status = authService.checkTwoStepStatus(email);
//			return ResponseEntity.ok(status);
//		} catch (Exception e) {
//			throw new ServerErrorException("SignIn failed with " + e.getMessage());
//		}
//	}

}
