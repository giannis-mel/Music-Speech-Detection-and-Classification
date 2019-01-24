in=1; % duration
over=0.5; % 50% overlap
k=1;

dirName = pwd;
files = dir( fullfile(dirName,'*.wav') );
f=1
fullFileName = fullfile(dirName, files(f).name);
duration=mirgetdata(mirlength(fullFileName));

for i=0:ceil((duration/over)-1) % division
        % pick the frame
        audio=miraudio(fullFileName,'Extract',over*i,over*i+win);

        % rms energy  
        AF(k,1)=mirgetdata(mirrms(audio));
        % zero cross
        AF(k,2)=mirgetdata(mirzerocross(audio));
        % statistical
        AF(k,3)=mirgetdata(mircentroid(audio));
        AF(k,4)=mirgetdata(mirspread(audio));
        AF(k,5)=mirgetdata(mirskewness(audio));
        AF(k,6)=mirgetdata(mirkurtosis(audio));
        AF(k,7)=mirgetdata(mirflatness(audio));
        AF(k,8)=mirgetdata(mirentropy(audio));

        % rollof, brightness
        AF(k,9)=mirgetdata(mirrolloff(audio,'Threshold',0.9));
        %AF(k,9)=mirgetdata(mirregularity(audio));
        AF(k,10)=mirgetdata(mirbrightness(audio,'CutOff',1500));

        % mfccs
        mfccs=mirgetdata(mirmfcc(audio));
        AF(k,11)= mfccs(1);
        AF(k,12)= mfccs(2);
        AF(k,13)= mfccs(3);
        AF(k,14)= mfccs(4);
        AF(k,15)= mfccs(5);
        AF(k,16)=mfccs(6);
        AF(k,17)=mfccs(7);
        AF(k,18)=mfccs(8);
        AF(k,19)=mfccs(9);
        AF(k,20)=mfccs(10);
        AF(k,21)=mfccs(11);
        AF(k,22)=mfccs(12);
        AF(k,23)=mfccs(13); 

        k=k+1;
end
    
output = array2table(AF);