package filmes.modelo;

import java.io.File;
import java.io.FileInputStream;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

public class CategoriasFilmes {
	private static final String DIR = "./filmes";
	private static Map<String, List<FilmeInfo>> categorias;

	private static void carregarCategorias() {
		categorias = new HashMap<String, List<FilmeInfo>>();
		File[] arqs = (new File(DIR)).listFiles();
		
		for(File arq : arqs)
			if(arq.isDirectory())
				categorias.put(arq.getName(), obterDiretorio(arq.getAbsolutePath()));
	}

	public static Map<String, List<FilmeInfo>> obterCategorias(){
		if(categorias == null)
			carregarCategorias();
		return Collections.unmodifiableMap(categorias);
	}
	
	private static List<FilmeInfo> obterDiretorio(String dir) {
		List<FilmeInfo> res = new ArrayList<FilmeInfo>();
		File[] arqs = new File(dir).listFiles();

		for (File arq : arqs) {
			if (arq.isFile()) {
				String conteudo = readFile(arq);
				try {
					FilmeInfo data = new Gson().fromJson(conteudo, FilmeInfo.class);
					res.add(data);
				} catch (Exception e) {
					// TODO: handle exception
				}
				
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
