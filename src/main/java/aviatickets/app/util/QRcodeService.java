package aviatickets.app.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import com.google.zxing.*;
import com.google.zxing.client.j2se.BufferedImageLuminanceSource;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.common.HybridBinarizer;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;
import org.springframework.stereotype.Component;

import javax.imageio.ImageIO;

@Component
public class QRcodeService {

	public void generateQR (Integer customerId) throws IOException, WriterException {

		//data that we want to store in the QR code
		String str= "THE HABIT OF PERSISTENCE IS THE HABIT OF VICTORY.";
		// path where we want to get QR Code
		String p = Paths.get(".").toAbsolutePath().normalize().toString();

		String path = p + "Ticket_" + customerId + ".png";
		// Encoding charset to be used
		String charset = "UTF-8";
//		Map<EncodeHintType, ErrorCorrectionLevel> hashMap = new HashMap<EncodeHintType, ErrorCorrectionLevel>();
////generates QR code with Low level(L) error correction capability
//		hashMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
//invoking the user-defined method that creates the QR code

		this.generateQRcode(str, path, charset, 300, 300);//increase or decrease height and width accordingly
		System.out.println("QR Code created successfully.");
	}

	public void readCode() throws NotFoundException, IOException {
		//path where the QR code is saved
		String path = "C:\\Users\\Anubhav\\Desktop\\QRDemo\\Quote.png";
//Encoding charset to be used
		String charset = "UTF-8";
		Map<EncodeHintType, ErrorCorrectionLevel> hintMap = new HashMap<EncodeHintType, ErrorCorrectionLevel>();
//generates QR code with Low level(L) error correction capability
		hintMap.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
		System.out.println("Data stored in the QR Code is: \n"+ this.readQRcode(path));
	}


	// #######################################################################################################33

	private void generateQRcode(String data, String path, String charset, Integer width, Integer height) throws WriterException, IOException
	{
//the BitMatrix class represents the 2D matrix of bits
//MultiFormatWriter is a factory class that finds the appropriate Writer subclass for the BarcodeFormat requested and encodes the barcode with the supplied contents.
		BitMatrix matrix = new MultiFormatWriter()
			.encode(
					new String(data.getBytes(charset), charset),
					BarcodeFormat.QR_CODE,
					width,
					height);
		MatrixToImageWriter.writeToFile(matrix, path.substring(path.lastIndexOf('.') + 1), new File(path));
	}



	//user-defined method that reads the QR code
	private String readQRcode(String path) throws FileNotFoundException, IOException, NotFoundException {
		BinaryBitmap binaryBitmap = new BinaryBitmap(new HybridBinarizer(new BufferedImageLuminanceSource(ImageIO.read(new FileInputStream(path)))));
		Result rslt = new MultiFormatReader().decode(binaryBitmap);
		return rslt.getText();
	}



}
