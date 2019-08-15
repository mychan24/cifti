write_nifti_2_hdr = function(hdr, filename) {

  fid = file(filename, open = "wb")
  hdr_size = as.integer(oro.nifti::sizeof_hdr(hdr))
  if (hdr_size != 540L) {
    close(fid)
    stop("Header size is not 540 - likely not a NIfTI-2 or CIFTI file!")
  }

  validate_length = function(name, len) {
    object = slot(hdr, name = name)
    if (length(object) != len) {
      stop(paste0("The ", name, " header field is not of length ", len))
    }
  }

  writeBin(as.integer(hdr@"sizeof_hdr"), fid, size=4)
  # writeChar(hdr@magic, fid, nchars = 8, eos = NULL)
  writeBin(as.integer(hdr@"magic"), fid, size=1) # MYC
  writeBin(as.integer(hdr@"datatype"), fid, size=2)
  writeBin(as.integer(hdr@"bitpix"), fid, size=2)

  # worked for int64
  validate_length("dim_", 8)
  writeBin(as.integer(hdr@"dim_"), fid, size = 8)
  writeBin(as.double(hdr@"intent_p1"), fid, size=8)
  writeBin(as.double(hdr@"intent_p2"), fid, size=8)
  writeBin(as.double(hdr@"intent_p3"), fid, size=8)
  writeBin(as.double(hdr@"pixdim"), fid, size=8)

  if (hdr@"vox_offset" < 544) {
    warning("vox_offset seems off!")
  }
  writeBin(as.integer(hdr@"vox_offset"), fid, size=8)

  writeBin(as.double(hdr@"scl_slope"), fid, size=8)
  writeBin(as.double(hdr@"scl_inter"), fid, size=8)
  writeBin(as.double(hdr@"cal_max"), fid, size=8)
  writeBin(as.double(hdr@"cal_min"), fid, size=8)


  writeBin(as.double(hdr@"slice_duration"), fid, size=8)
  writeBin(as.double(hdr@"toffset"), fid, size=8)


  writeBin(as.integer(hdr@"slice_start"), fid, size=8)
  writeBin(as.integer(hdr@"slice_end"), fid, size=8)

  writeChar(hdr@descrip, fid, nchars = 80, eos = NULL)
  writeChar(hdr@aux_file, fid, nchars = 24, eos = NULL)

  writeBin(as.integer(hdr@"qform_code"), fid, size=4)
  writeBin(as.integer(hdr@"sform_code"), fid, size=4)

  writeBin(as.double(hdr@"quatern_b"), fid, size=8)
  writeBin(as.double(hdr@"quatern_c"), fid, size=8)
  writeBin(as.double(hdr@"quatern_d"), fid, size=8)
  writeBin(as.double(hdr@"qoffset_x"), fid, size=8)
  writeBin(as.double(hdr@"qoffset_y"), fid, size=8)
  writeBin(as.double(hdr@"qoffset_z"), fid, size=8)

  validate_length("srow_x", 4)
  validate_length("srow_y", 4)
  validate_length("srow_z", 4)

  writeBin(as.double(hdr@"srow_x"), fid, size=8)
  writeBin(as.double(hdr@"srow_y"), fid, size=8)
  writeBin(as.double(hdr@"srow_z"), fid, size=8)

  writeBin(as.integer(hdr@"slice_code"), fid, size=4)
  writeBin(as.integer(hdr@"xyzt_units"), fid, size=4)
  writeBin(as.integer(hdr@"intent_code"), fid, size=4)

  writeChar(hdr@intent_name, fid, nchars = 16, eos = NULL)
  writeChar(hdr@dim_info, fid, nchars = 1, eos = NULL)

  suppressWarnings({
    writeChar("", fid, nchars = 15, eos = NULL)
  })

  nhdr = seek(fid)
  stopifnot(nhdr == hdr_size)
  return(list(fid = fid, filename = filename))

}





write_cifti = function(res) {

  #filename = tempfile()
  filename = res$filename

  ### XML
  source("./sample_xml_in_header.R") # Source a script that makes the hard-coded xml object 'cml'
  cml_string <- toString.XMLNode(cml)
  cml_string <- rm_white_bracket(text.var = cml_string)
  cml_string <- cml_string %>% stringr::str_replace_all(c(" <" = "<",
                                                          "> " = ">")) # remove lead/trailing space
  cml_string <- rm_white_multiple(text.var = cml_string)
  cml_string <- rm_white_bracket(text.var = cml_string)

  xmlsize <- nchar(cml_string)
  xmlpad  <- round((xmlsize+8)/16)*16 - (xmlsize+8) # to pad it to 16bit

  #### ==== Make/edit header === ####
  hdr = res$hdr
  hdr@vox_offset <- 4+540+8+xmlsize+xmlpad # calculate voxel offset based on xml header size
  # *** the rest of hdr should be defined here later MYC


  L = write_nifti_2_hdr(hdr, filename = filename) # this writes 4 + 540 bytes of data
  fid = L$fid

  ##   Write header extension
  # 4 bytes with the size of the header, 384 for nifti-1 or 540 for nifti-2
  # 540 bytes with the nifti-2 header
  # 4 bytes that indicate the presence of a header extension [1 0 0 0]
  # 4 bytes with the size of the header extension in big endian?
  # 4 bytes with the header extension code NIFTI_ECODE_CIFTI [0 0 0 32]
  # variable number of bytes with the xml section, at the end there might be some empty "junk"
  # 8 bytes, presumaby with the size and type?
  # variable number of bytes with the voxel data
  # the size of the header extension must be an integer multiple of 16 bytes according to http://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/extension.html

  seek(fid, where = 540, origin = "start")
  writeBin(as.integer(c(1,0,0,0)), fid, size=1) # indicate the presence of a header extension
  writeBin(as.integer((8+xmlsize+xmlpad)), fid, size=4) # esize = 8 (bytes for esize and etype) + xmlsize + xmlpad
  writeBin(as.integer(32), fid, size=4) # etype
  writeChar(cml_string, fid, nchars = xmlsize, eos=NULL) # XML section
  writeBin(as.integer(rep(0, xmlpad)), fid, size=4) # zero-pad to the next 16 byte boundary

  #### ==== Make/edit data === ####

  data = res$data

  # reversing things that happened with read_cifti
  # ddata = dim(data)  # if just entering something with $data?
  ddata = attr(res$data, "orig_dim") # MYC - if entering a cifti object??
  trans_data = attr(data, "trans")
  if (is.null(trans_data)) {
    trans_data = FALSE
  }
  if (trans_data) {
    if (length(ddata) == 2) {
      data = t(data)
    } else {
      trans_data = FALSE
      warning("Dimensions of the data > 2, so no transposing done!")
    }
  }

  # reversing things that happened with read_cifti
  drop_data = attr(data, "drop")
  if (is.null(drop_data)) {
    drop_data = FALSE
  }
  orig_dim = attr(data, "orig_dim")
  if (drop_data) {
    data = array(data, dim = orig_dim)
  }

  # seek(fid, where = hdr@vox_offset, origin = "start"); # myc commented out

  dtype = as.character(hdr@datatype)
  what_func = switch(
    dtype,
    "2" = as.integer,
    "4" = as.numeric,
    "8" = as.integer,
    "16" = as.numeric,
    "64" = as.double,
    "512" = as.numeric,
    "768" = as.integer
  )

  size = switch(
    dtype,
    "2" = 1,
    "4" = 2,
    "8" = 4,
    "16" = 4,
    "64" = 8,
    "512" = 2,
    "768" = 4
  )
  if (is.null(what_func) || is.null(size)) {
    stop("Unsupported data type indicated by NIfTI-2 header!")
  }

  vals = c(data)
  img_dim = hdr@dim_
  n_items = prod(img_dim[2:length(img_dim)])
  if (n_items > length(vals)) {
    stop("Not all CIFTI data read!")
  }
  if (n_items < length(vals)) {
    stop("More data read than header indicated - header or data problem!")
  }

  cifti_dim = img_dim[6:8]
  vals = array(vals, dim = cifti_dim)

  # Write data
  writeBin(object = as.double(vals), con = fid, size = size) # MYC

  close(fid)
}