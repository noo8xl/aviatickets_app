package aviatickets.app.auth;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import com.google.zxing.WriterException;

import java.io.IOException;
import java.sql.SQLException;

interface AuthInterface {

  SignInResponse signIn(SignInDto dto) throws SQLException, ClassNotFoundException;

	Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException;

  void signUp(SignUpDto dto) throws SQLException, ClassNotFoundException, IOException, WriterException;

  void forgotPassword(String email) throws SQLException, ClassNotFoundException;
}
