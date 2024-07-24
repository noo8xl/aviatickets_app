package aviatickets.app.auth;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;

import java.sql.SQLException;

interface AuthInteraction {
  // login
  SignInResponse signIn(SignInDto dto) throws SQLException, ClassNotFoundException;

	// checkTwoStepStatus -> check if customer enabled 2fa has
	Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException;

  // registration
  void signUp(SignUpDto dto) throws SQLException, ClassNotFoundException;

  // send new password to customer
  void forgotPassword(String email) throws SQLException, ClassNotFoundException;
}
