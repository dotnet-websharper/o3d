<%@ Page Language="C#" AutoEventWireup="true" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
   <title></title>
   <WebSharper:ScriptManager runat="server" ></WebSharper:ScriptManager>
   <script type="text/javascript">
       o3djs.require('o3djs.webgl');
       o3djs.require('o3djs.util');
       o3djs.require('o3djs.math');
       o3djs.require('o3djs.rendergraph');
       o3djs.require('o3djs.primitives');
       o3djs.require('o3djs.quaternions');
       o3djs.require('o3djs.effect');
       o3djs.require('o3djs.event');
   </script>
 </head>
<body>
    <form id="form1" runat="server">
    <div>
    <h1>Test</h1>
    <Samples:Samples runat="server" ></Samples:Samples>
    </div>
    </form>
</body>
</html>
