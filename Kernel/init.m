(** User Mathematica initialization file **)
Notation`AutoLoadNotationPalette = False
Needs["Notation`"]

Notation[ParsedBoxWrapper[SubscriptBox["t_", "i_"]] \[DoubleLongLeftRightArrow] 
   ParsedBoxWrapper[RowBox[{"Indexed", "[", 
      RowBox[{"t_", ",", RowBox[{"List", "[", "i_", "]"}]}], 
      "]"}]]]

Alert[]:=EmitSound[Sound[SoundNote[]]]

(* File: init.m *)

(*
	This kernel init file makes the Mathematica 6 kernel create front end links that are suitable
	for SSH port forwarding from a front end to a remote kernel. The MathLink connections between
	front end and kernel can be tunneled over SSH port 22. This makes it possible to connect
	the front end to a remote kernel behind a firewall or a NAT router over the internet.

	Created February 2008 by Sascha Kratky, kratky@unisoftwareplus.com

	Installation:
	Install this file named init.m into the directory $BaseDirectory/Kernel/ on the remote
	kernel machine.

	$BaseDirectory usually is
	C:\Documents and Settings\All Users\Application Data\Mathematica under Windows,
	/Library/Mathematica under Mac OS X,
	/usr/share/Mathematica under Linux.

	The accompanying batch files tunnel.sh and tunnel.bat should be copied to the directory
	$UserBaseDirectory/FrontEnd on the front end machine.

	$UserBaseDirectory usually is
	C:\Documents and Settings\your_user_name\Application Data\Mathematica under Windows,
	~/Library/Mathematica under Mac OS X,
	~/.Mathematica under Linux.

	Configuration:
	On the front end machine configure the kernel configuration options as follows:
	Arguments To MLOpen:
	-LinkMode Listen -LinkProtocol TCPIP -LinkOptions MLDontInteract -LinkHost 127.0.0.1

	Launch command for Windows front end:
	"`userbaseDirectory`\FrontEnd\tunnel.bat" [user[:password]@]remote_machine[:port] "remote_kernel_path" "`linkname`"

	Launch command for Mac OS X, Linux front end:
	"`userbaseDirectory`/FrontEnd/tunnel.sh" [user[:password]@]remote_machine[:port] "remote_kernel_path" "`linkname`"

	In the above launch command remote_machine must be replaced with the DNS name
	or the IP address of the machine that runs the remote Mathematica 6 kernel.
	remote_kernel_path must be replaced with the remote kernel installation location:
	C:\Program Files\Wolfram Research\Mathematica\6.0\math.exe for Windows
	/Applications/Mathematica.app/Contents/MacOS/MathKernel for Mac OS X,
	/usr/local/bin/math for Linux).

	If the password is not specified as part of the first argument, the sshd server on the remote
	kernel machine has to be configured to allow for password-less SSH logins. You may also need
	to run an SSH authentication agent (ssh-agent or Pageant) on the front end machine.
	
	The batch files tunnel.sh and tunnel.bat generate a log file named tunnel.log in the
	$UserBaseDirectory/FrontEnd folder. This log file contains troubleshooting information,
	which is helpful if the connection to the remote kernel fails.

	Discussion:
	The Mathematica 6 front end and kernel communicate with each other through several
	MathLink connections, known as the main link, the preemptive link, and the service link.
	If the kernel and the front end run on different machines, the TCPIP MathLink protocol is
	used for those links. Each TCPIP link requires two open TCP/IP channels between the local
	machine and the remote machine. One channel functions as the primary data stream and
	the second channel functions as the urgent message channel.

	When a remote kernel is started from the front end, the main link is established as
	a callback connection from the kernel machine to the front end machine. During startup
	the kernel sets up the preemptive and the service link as listening links by calling
	the function MathLink`CreateFrontEndLink for both links. The front end then establishes
	connections to both links.

	In order for the front end to properly connect to the remote kernel SSH port forwarding has
	to be configured for each one of the 6 TCP/IP channels. The TCP/IP ports used for the main
	link are forwarded from the remote kernel machine to the local front end machine.
	Forwarding the TCP/IP channels of the preemptive and the service link is more tricky.
	This kernel init file replaces MathLink`CreateFrontEndLink with an alternate implementation
	that makes the kernel use forwarded TCP/IP ports for the preemptive and the service link.
*)

MathLink`CreateFrontEndLinkHost[]:=Module[
	{pos,linkName,linkNameComponents,linkHost,IPAddrToInteger,candidates},
	If[ValueQ[$ParentLink] && Head[$ParentLink] === LinkObject,
		(* extract linkHost from parent link *)
		linkName = $ParentLink[[1]];
		linkNameComponents = StringSplit[ linkName , {"@", ","} ];
		(* if not a TCPIP linkname, default to automatic selection of LinkHost *)
		If [ Length[linkNameComponents] != 4, Return[Automatic]];
		linkHost = linkNameComponents[[2]],
	(*Else*)
		(* search for -linkhost option on command line *)
		pos=Position[ToLowerCase[$CommandLine], "-linkhost"];
		pos=If[Length[pos]==1,pos[[1]][[1]] +1,Length[$CommandLine] + 1];
		(* if no -linkhost option on command line, default to automatic selection of LinkHost *)
		If[pos>Length[$CommandLine],Return[Automatic]];
		linkHost=$CommandLine[[pos]]
	];
	(* if linkhost is the loopback interface, we are done *)
	If[linkHost=="127.0.0.1", Return[linkHost]];
	(* heuristic: search for best matching address in $MachineAddresses on the same subnet *)
	IPAddrToInteger := FromDigits[ToExpression[StringSplit[#,"."]],256]&;
	candidates = BitXor[IPAddrToInteger /@ $MachineAddresses, IPAddrToInteger @ linkHost]
		// Ordering[#, 1]&
		// Part[ $MachineAddresses,#]&;
	If[Length[candidates]>0,
		(* best candidate is first in list *)
		First[candidates],
	(*Else*)
		(* no candidate, default to Automatic selection of LinkHost *)
		Return[Automatic]
	]
]

MathLink`CreateFrontEndLinkName[]:=Module[
	{pos,linkName,linkNameComponents},
	If [ !ValueQ[MathLink`$PortNumber],
		If[ValueQ[$ParentLink] && Head[$ParentLink] === LinkObject,
			(* extract linkHost from parent link *)
			linkName = $ParentLink[[1]],
		(*Else*)
			(* search for -linkname option on command line *)
			pos = Position[ ToLowerCase[$CommandLine], "-linkname" ];
			pos = If[ Length[pos]==1,pos[[1]][[1]] +1, Length[$CommandLine] + 1];
			(* if no -linkname option on command line, default to automatic selection of LinkName *)
			If[ pos>Length[$CommandLine], Return[Automatic] ];
			linkName=$CommandLine[[pos]]
		];
		linkNameComponents = StringSplit[ linkName , {"@", ","} ];
		(* check if link has been created on loopback interface *)
		If [ Length[linkNameComponents] === 4 && linkNameComponents[[2]] === "127.0.0.1",
			(* initialize port number from main link name *)
			(* port numbers beyond the parsed one are assumed to be properly forwarded over SSH *)
			MathLink`$PortNumber = Max[
				ToExpression[linkNameComponents[[1]]],
				ToExpression[linkNameComponents[[3]]]
				],
		(*Else*)
			Return[Automatic]
		]
	];
	Return [ StringJoin[
		{ToString[++MathLink`$PortNumber], "@127.0.0.1,",
		ToString[++MathLink`$PortNumber], "@127.0.0.1"}]
	]
]

Unprotect[MathLink`CreateFrontEndLink]

MathLink`CreateFrontEndLink[] := Module[ {linkName},
	linkName = MathLink`CreateFrontEndLinkName[];
	If [ linkName === Automatic,
		LinkCreate[ 
			LinkMode->Listen,
			LinkProtocol->MathLink`CreateFrontEndLinkProtocol[], 
 			LinkHost -> MathLink`CreateFrontEndLinkHost[] ],
	(*Else*)
		LinkCreate[ 
			linkName, 
			LinkMode->Listen, 
			LinkProtocol->"TCPIP" ]
	]
]

Protect[MathLink`CreateFrontEndLink]
