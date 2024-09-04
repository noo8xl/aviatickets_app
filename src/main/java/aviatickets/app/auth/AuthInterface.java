package aviatickets.app.auth;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import com.google.zxing.WriterException;

import java.io.IOException;
import java.net.URISyntaxException;
import java.sql.SQLException;

interface AuthInterface {

	// login area
  SignInResponse signIn(SignInDto dto) throws SQLException, ClassNotFoundException;

	// check customer 2fa status by email and if true - send code to the client
	Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException;

	//
	void sendTwoStepCodeToTheCustomer(String email) throws SQLException, ClassNotFoundException;

	// registration for the new customer
  void signUp(SignUpDto dto) throws SQLException, ClassNotFoundException, IOException, WriterException;

	// renew password via send it to the customer email address
  void forgotPassword(String email) throws SQLException, ClassNotFoundException, URISyntaxException;
}
