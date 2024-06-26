use actix_web::{get, post, web, App, HttpServer, HttpResponse, Result, Responder, 
	http::header::ContentType};
use serde::{Serialize, Deserialize};


const LOCALHOST: &str = "127.0.0.1";
const LOCALPORT: u16 = 50501;


struct ProdFrcst {
    prod_id: String,
    prod_name: String,
    frcst_date: String,
	frcst_quan: u32
}


#[derive(Serialize)]
struct ProdFrcstOut {
    prod_id: String,
    prod_name: String,
    frcst_date: String,
	frcst_quan: u32
}

impl ProdFrcstOut {
		fn new() -> ProdFrcstOut {
			ProdFrcstOut {
				prod_id: "".to_string(),
				prod_name: "".to_string(),
				frcst_date: "".to_string(),
				frcst_quan: 0,
			}
		}
}

#[derive(Deserialize)]
struct SelParamsLine {
    prod_id: String,
	frcst_date: String,
}

#[derive(Deserialize)]
struct SelParams {
    seltab: Vec<SelParamsLine>,
}

#[get("/forecast/sales/{prod_id}/{yyyy_mm_dd}")] 
async fn frcst4prod(path: web::Path<(String, String)>) -> Result<String> {
    let (prod_id_trg, date_trg) = path.into_inner();
	let forecastdb = forecast_db();
	for _each in &forecastdb {
		if _each.prod_id == prod_id_trg 
			&& _each.frcst_date == date_trg 
			{ 
			return Ok(format!("Forecast for product {} on {} : {}", _each.prod_id, _each.frcst_date, _each.frcst_quan));
		}
	}
	Ok(format!("Not found forecast for {} {}", prod_id_trg, date_trg))
    
}


#[get("/api/v1/forecast/sales/{prod_id}/{yyyy_mm_dd}")] 
async fn api_frcst4prod(path: web::Path<(String, String)>) -> Result<impl Responder> {
    let (prod_id_trg, date_trg) = path.into_inner();
	let mut frcst_resp_out = ProdFrcstOut::new();
	let forecastdb = forecast_db();
	for _each in &forecastdb {
		if _each.prod_id == prod_id_trg 
			&& _each.frcst_date == date_trg 
			{ 
				frcst_resp_out.prod_id = _each.prod_id.clone();
				frcst_resp_out.prod_name = _each.prod_name.clone();
				frcst_resp_out.frcst_date = _each.frcst_date.clone();
				frcst_resp_out.frcst_quan = _each.frcst_quan;
		}
	}
	Ok(web::Json(frcst_resp_out))
    
}

#[post("/api/v1/api_frcstlist")] 
async fn api_frcstlist(sel_params: web::Json<SelParams>) -> Result<impl Responder> {
    //let (prod_id_trg, date_trg) = path.into_inner();
	let mut frcst_lines: Vec<ProdFrcstOut> = Vec::new();
	let forecastdb = forecast_db();
	for _each_sel_line in &sel_params.seltab {
		for _each_db in &forecastdb {
			if _each_db.prod_id == _each_sel_line.prod_id {
				if _each_sel_line.frcst_date == "" {
					let mut frcst_line_out = ProdFrcstOut::new();
					frcst_line_out.prod_id = _each_db.prod_id.clone();
					frcst_line_out.prod_name = _each_db.prod_name.clone();
					frcst_line_out.frcst_date = _each_db.frcst_date.clone();
					frcst_line_out.frcst_quan = _each_db.frcst_quan;
					frcst_lines.push(frcst_line_out);
				} else {
					if _each_sel_line.frcst_date == _each_db.frcst_date {
					let mut frcst_line_out = ProdFrcstOut::new();
					frcst_line_out.prod_id = _each_db.prod_id.clone();
					frcst_line_out.prod_name = _each_db.prod_name.clone();
					frcst_line_out.frcst_date = _each_db.frcst_date.clone();
					frcst_line_out.frcst_quan = _each_db.frcst_quan;
					frcst_lines.push(frcst_line_out);
					}
				}
			}
				
			}
		}

	Ok(web::Json(frcst_lines))
    
}


#[get("/")] 
async fn index() -> HttpResponse {
let resp_str = short_info();
	HttpResponse::Ok()
		.content_type(ContentType::html())
		.body(resp_str.unwrap())
}

async fn index2() -> HttpResponse  {
	let resp_str = short_info();
	HttpResponse::Ok()
		.content_type(ContentType::html())
		.body(resp_str.unwrap())
}

fn forecast_db() -> Vec<ProdFrcst> {
	vec![
		ProdFrcst {
			prod_id: "PROD01".to_string(),
			prod_name: "Первоклассный Полезный продукт".to_string(),
			frcst_date: "2024-06-06".to_string(),
			frcst_quan: 20,
		},
		ProdFrcst {
			prod_id: "PROD01".to_string(),
			prod_name: "Первоклассный Полезный продукт".to_string(),
			frcst_date: "2024-06-07".to_string(),
			frcst_quan: 10,
		},
		ProdFrcst {
			prod_id: "PROD01".to_string(),
			prod_name: "Первоклассный Полезный продукт".to_string(),
			frcst_date: "2024-06-08".to_string(),
			frcst_quan: 15,
		},
		ProdFrcst {
			prod_id: "PROD02".to_string(),
			prod_name: "Полезный продукт".to_string(),
			frcst_date: "2024-06-06".to_string(),
			frcst_quan: 50,
		},
		ProdFrcst {
			prod_id: "PROD02".to_string(),
			prod_name: "Полезный продукт".to_string(),
			frcst_date: "2024-06-07".to_string(),
			frcst_quan: 75,
		},
	]
	
}

fn short_info() -> Result<String> {
Ok("Доступные методы
	<BR>	/forecast/sales/{prod_id} [get]
	<BR>	/forecast/sales/{prod_id} [get]
	<BR>	/forecast/sales_list [post/body_json]
".to_string())
}



// ===== comment line
#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::
	new(|| {
	App::new()
			.service(index)
			.service(frcst4prod)
			.service(api_frcst4prod)
			.service(api_frcstlist)
			.default_service(
				web::get().to(index2)
			)
	})
       .bind((LOCALHOST, LOCALPORT))?
       .run()
       .await
}
