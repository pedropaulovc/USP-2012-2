package filmes.modelo;

import java.io.File;
import java.io.FileInputStream;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.google.gson.Gson;

public class CategoriasFilmes {
	private static List<FilmeInfo> comedia = new ArrayList<FilmeInfo>();
	private static List<FilmeInfo> drama = new ArrayList<FilmeInfo>();
	private static List<FilmeInfo> acao = new ArrayList<FilmeInfo>();

	public static List<FilmeInfo> obterComedia() {
		if (comedia.size() == 0)
			comedia = obterDiretorio("./filmes/comedia");
		return comedia;
	}

	public static List<FilmeInfo> obterAcao() {
		if (acao.size() == 0)
			acao = obterDiretorio("./filmes/acao");
		return acao;
	}

	public static List<FilmeInfo> obterDrama() {
		if (drama.size() == 0)
			drama = obterDiretorio("./filmes/drama");
		return drama;
	}

	private static List<FilmeInfo> obterDiretorio(String dir) {
		List<FilmeInfo> res = new ArrayList<FilmeInfo>();
		File[] arqs = new File(dir).listFiles();

		for (File arq : arqs) {
			if (arq.isFile()) {
				String conteudo = readFile(arq);
				FilmeInfo data = new Gson().fromJson(conteudo, FilmeInfo.class);
				res.add(data);
			}
		}

		return res;
	}

	private static String readFile(File file) {
		try {
			FileInputStream stream = new FileInputStream(file);
			FileChannel fc = stream.getChannel();
			MappedByteBuffer bb = fc.map(FileChannel.MapMode.READ_ONLY, 0, fc
					.size());
			/* Instead of using default, pass in a decoder. */
			stream.close();
			return Charset.defaultCharset().decode(bb).toString();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
		}
		return null;
	}


}
