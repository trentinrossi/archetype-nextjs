'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { userSecurityService } from '@/services/userSecurityService';
import { SignonRequestDTO, SignonResponseDTO, ExitResponseDTO, InvalidKeyResponseDTO } from '@/types/user-security';

interface SignonScreenProps {
  programName?: string;
  transactionId?: string;
  systemId?: string;
  workstationId?: string;
}

export default function SignonScreen({
  programName = 'COSGN00C',
  transactionId = 'SIGNON',
  systemId = 'SYSTEM',
  workstationId = 'WS001'
}: SignonScreenProps) {
  const router = useRouter();
  
  // Form state
  const [userId, setUserId] = useState<string>('');
  const [password, setPassword] = useState<string>('');
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>('');
  const [showThankYou, setShowThankYou] = useState<boolean>(false);
  
  // Screen state
  const [currentDateTime, setCurrentDateTime] = useState<string>('');
  const [focusedField, setFocusedField] = useState<'userId' | 'password'>('userId');

  // Update current date and time
  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const dateStr = now.toLocaleDateString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit'
      });
      const timeStr = now.toLocaleTimeString('en-US', {
        hour12: false,
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit'
      });
      setCurrentDateTime(`${dateStr} ${timeStr}`);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    
    return () => clearInterval(interval);
  }, []);

  // Handle authentication
  const handleAuthentication = useCallback(async () => {
    if (!userId.trim()) {
      setError('User ID is required');
      setFocusedField('userId');
      return;
    }

    if (!password.trim()) {
      setError('Password is required');
      setFocusedField('password');
      return;
    }

    setLoading(true);
    setError('');

    try {
      const signonRequest: SignonRequestDTO = {
        userId: userId.trim().toUpperCase(),
        password: password,
        systemId,
        workstationId,
        transactionId
      };

      const response: SignonResponseDTO = await userSecurityService.signon(signonRequest);

      if (response.success) {
        // Check if password change is required
        if (response.passwordChangeRequired) {
          router.push('/change-password');
          return;
        }

        // Determine redirect path based on user type
        const redirectPath = userSecurityService.getRedirectPath(response.userSecurity);
        
        // Show success message briefly before redirect
        if (response.message) {
          setError(''); // Clear any previous errors
          setTimeout(() => {
            router.push(redirectPath);
          }, 1000);
        } else {
          router.push(redirectPath);
        }
      } else {
        setError(response.message || 'Authentication failed');
        setPassword('');
        setFocusedField('password');
      }
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Authentication failed';
      setError(errorMessage);
      setPassword('');
      setFocusedField('password');
    } finally {
      setLoading(false);
    }
  }, [userId, password, systemId, workstationId, transactionId, router]);

  // Handle PF3 exit
  const handleExit = useCallback(async () => {
    setLoading(true);
    
    try {
      const exitRequest = {
        userId: userId || 'GUEST',
        sessionId: userSecurityService.getStoredSessionId() || '',
        sessionToken: typeof window !== 'undefined' ? localStorage.getItem('access_token') || '' : ''
      };

      const response: ExitResponseDTO = await userSecurityService.exit(exitRequest);
      
      if (response.success) {
        setShowThankYou(true);
        setTimeout(() => {
          if (typeof window !== 'undefined') {
            window.close();
          }
        }, 3000);
      }
    } catch (err) {
      console.error('Exit failed:', err);
      setShowThankYou(true);
      setTimeout(() => {
        if (typeof window !== 'undefined') {
          window.close();
        }
      }, 3000);
    } finally {
      setLoading(false);
    }
  }, [userId]);

  // Handle invalid key
  const handleInvalidKey = useCallback(async (key: string) => {
    try {
      const invalidKeyRequest = {
        userId: userId || 'GUEST',
        invalidKey: key,
        reason: `Invalid function key pressed: ${key}`
      };

      const response: InvalidKeyResponseDTO = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.acknowledged) {
        setError(`Invalid key: ${key}. ${response.message}`);
      }
    } catch (err) {
      setError(`Invalid key: ${key}`);
    }
  }, [userId]);

  // Handle keyboard events
  const handleKeyDown = useCallback((event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!loading) {
          handleAuthentication();
        }
        break;
      case 'F3':
        event.preventDefault();
        if (!loading) {
          handleExit();
        }
        break;
      case 'F1':
      case 'F2':
      case 'F4':
      case 'F5':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'F12':
        event.preventDefault();
        handleInvalidKey(event.key);
        break;
      case 'Tab':
        if (focusedField === 'userId') {
          setFocusedField('password');
        } else {
          setFocusedField('userId');
        }
        break;
      default:
        break;
    }
  }, [loading, handleAuthentication, handleExit, handleInvalidKey, focusedField]);

  // Handle input changes
  const handleUserIdChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const value = event.target.value.toUpperCase();
    setUserId(value);
    setError('');
  };

  const handlePasswordChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setPassword(event.target.value);
    setError('');
  };

  // Show thank you message
  if (showThankYou) {
    return (
      <div className="min-h-screen bg-black text-green-400 font-mono flex items-center justify-center">
        <div className="text-center">
          <div className="text-2xl mb-4">Thank you for using the system</div>
          <div className="text-lg">Session ended successfully</div>
          <div className="text-sm mt-4 text-green-300">Window will close automatically...</div>
        </div>
      </div>
    );
  }

  return (
    <div 
      className="min-h-screen bg-black text-green-400 font-mono p-4"
      onKeyDown={handleKeyDown}
      tabIndex={0}
    >
      {/* Screen Header */}
      <div className="border border-green-400 mb-4 p-2">
        <div className="flex justify-between items-center">
          <div className="flex space-x-8">
            <span>Program: {programName}</span>
            <span>Transaction: {transactionId}</span>
          </div>
          <div>
            <span>{currentDateTime}</span>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-2xl mx-auto">
        {/* Title */}
        <div className="text-center mb-8">
          <h1 className="text-2xl mb-2">SYSTEM SIGN-ON</h1>
          <div className="text-sm text-green-300">Enter your credentials to access the system</div>
        </div>

        {/* Sign-on Form */}
        <div className="border border-green-400 p-6 mb-4">
          <div className="space-y-6">
            {/* User ID Field */}
            <div className="flex items-center">
              <label className="w-24 text-right mr-4">User ID:</label>
              <div className="flex-1">
                <Input
                  type="text"
                  value={userId}
                  onChange={handleUserIdChange}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono uppercase"
                  maxLength={8}
                  placeholder="Enter User ID"
                  disabled={loading}
                  autoFocus={focusedField === 'userId'}
                />
              </div>
            </div>

            {/* Password Field */}
            <div className="flex items-center">
              <label className="w-24 text-right mr-4">Password:</label>
              <div className="flex-1">
                <Input
                  type="password"
                  value={password}
                  onChange={handlePasswordChange}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={20}
                  placeholder="Enter Password"
                  disabled={loading}
                  autoFocus={focusedField === 'password'}
                />
              </div>
            </div>
          </div>
        </div>

        {/* Error Message */}
        {error && (
          <div className="border border-red-400 bg-red-900/20 text-red-400 p-3 mb-4">
            <div className="font-bold">ERROR:</div>
            <div>{error}</div>
          </div>
        )}

        {/* Loading Message */}
        {loading && (
          <div className="border border-yellow-400 bg-yellow-900/20 text-yellow-400 p-3 mb-4">
            <div>Processing... Please wait</div>
          </div>
        )}

        {/* Function Keys */}
        <div className="border border-green-400 p-4">
          <div className="text-sm">
            <div className="mb-2 font-bold">Function Keys:</div>
            <div className="grid grid-cols-2 gap-2">
              <div>ENTER = Sign On</div>
              <div>F3 = Exit</div>
            </div>
          </div>
        </div>

        {/* Action Buttons */}
        <div className="flex justify-center space-x-4 mt-6">
          <Button
            onClick={handleAuthentication}
            disabled={loading || !userId.trim() || !password.trim()}
            className="bg-green-700 hover:bg-green-600 text-white border-green-400 font-mono"
          >
            {loading ? 'Signing On...' : 'Sign On (ENTER)'}
          </Button>
          
          <Button
            onClick={handleExit}
            disabled={loading}
            variant="outline"
            className="border-green-400 text-green-400 hover:bg-green-900/20 font-mono"
          >
            Exit (F3)
          </Button>
        </div>

        {/* System Information */}
        <div className="mt-8 text-center text-xs text-green-300">
          <div>System ID: {systemId} | Workstation: {workstationId}</div>
          <div className="mt-2">© 2025 System Security Module</div>
        </div>
      </div>
    </div>
  );
}