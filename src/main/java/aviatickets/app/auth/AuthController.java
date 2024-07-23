package aviatickets.app.auth;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.CustomerService;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.ServerErrorException;
import jakarta.validation.Valid;
import org.springframework.web.servlet.HandlerMapping;

import java.sql.SQLException;

@RestController
@RequestMapping("/auth")
public class AuthController {
  private final AuthService authService;
  private final CustomerService customerService;
	private final HandlerMapping resourceHandlerMapping;

	public AuthController(AuthService authService, CustomerService customerService, @Qualifier("resourceHandlerMapping") HandlerMapping resourceHandlerMapping) {
		this.authService = authService;
		this.customerService = customerService;
		this.resourceHandlerMapping = resourceHandlerMapping;
	}

	// signUp -> registration for the new customer
	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/signUp/")
	void signUp(@Valid @RequestBody SignUpDto dto) {
		authService.signUp(dto);
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


	// signIn -> login area
	@ResponseStatus(HttpStatus.OK)
	@PostMapping("/signIn/")
	ResponseEntity<SignInResponse> signIn(@Valid @RequestBody SignInDto dto) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(authService.signIn(dto));
	}

  // forgotPassword -> send new pwd to user email
  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/forgot-password/{email}")
  void forgotPassword(@PathVariable String email) {
		authService.forgotPassword(email);
  }

}
