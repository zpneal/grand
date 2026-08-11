This file describes the data contained in scoping_public.Rdata.

COLLECTION
The file contains data collected via two separate web-based surveys, identified by the 'survey' variable. Both surveys were approved as exempt by the Michigan State University Institutional Review Board on 21 February 2023 (Study ID: STUDY00008880).

The "board" survey sought to include all members of the boards of two network associations (INSNA and NetSci) and seven network journals (\textit{Applied Network Science}, \textit{Connections}, \textit{Journal of Complex Networks}, \textit{Journal of Social Structure}, \textit{Network Science}, \textit{Social Networks}, \textit{Social Network Analysis and Mining}). An invitation to participate was sent to the 237 board members with valid email addresses on 15 March 2023. Non-respondents received weekly email reminders until the survey closed on 20 April 2023.

The "web" survey was a convenience sample, where participants were recruited by invitations circulated via social media (Mastodon, Twitter) and listservs (SOCNET, REDES (in Spanish), cna2023, siam-ns) in May and June 2023.

SURVEYS
The verbatim surveys are located in "scpoing_board.docx" and "scoping_web.docx", which also provide variable names. The two surveys are nearly identical, but the web survey contained some additional questions. 

DATA CLEANING
The raw data was processed into a cleaned public release file using "scoping_cleaning.R". Some lines of this code have been redacted to preserve respondents' anonymity. The gender of non-binary respondents (N = 4) is suppressed to preserve anonymity.

CONSTRUCTED VARIABLES
The variable 'gender_r' contains respondents' reported gender. When this value was missing and the respondent was personally known to Zachary Neal, this variable contains an imputed gender.

The variables 'field_r1' and 'field_r2' are harmonized re-codes of the open-ended 'field'. 

NON-PUBLIC DATA
Additional identifying information and non-public variables were collected (see the survey instruments). These data may be available under certain circumstances.

QUESTIONS
For any questions about these data, please contact Zachary Neal at zpneal@msu.edu.