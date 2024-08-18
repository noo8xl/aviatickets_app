package aviatickets.app.auth;

import java.io.IOException;
import java.net.URISyntaxException;
import java.sql.SQLException;

import com.google.zxing.WriterException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import jakarta.validation.Valid;

@RequiredArgsConstructor
@RestController
@RequestMapping("/auth")
public class AuthController {

  private final AuthInterface authService;

	@ResponseStatus(HttpStatus.CREATED)
	@PostMapping("/signUp/")
	void signUp(@Valid @RequestBody SignUpDto dto) throws SQLException, ClassNotFoundException, IOException, WriterException {
		this.authService.signUp(dto);
	}

	@ResponseStatus(HttpStatus.OK)
	@PostMapping("/signIn/")
	ResponseEntity<SignInResponse> signIn(@Valid @RequestBody SignInDto dto) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.authService.signIn(dto));
	}

  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/forgot-password/{email}/")
  void forgotPassword(@PathVariable String email) throws SQLException, ClassNotFoundException, URISyntaxException {
		this.authService.forgotPassword(email);
  }

	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/check-2fa/{email}/")
	ResponseEntity<Boolean> check2faStatus(@Valid @PathVariable String email) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.authService.checkTwoStepStatus(email));
	}

}
