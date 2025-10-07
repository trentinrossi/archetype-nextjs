'use client';

import React, { useState } from 'react';
import { Button } from '@/components/ui/Button';
import { SignonRequestDTO, SignonResponseDTO } from '@/types/user';
import { userService } from '@/services/userService';

interface SignonFormProps {
  onSignonSuccess: (response: SignonResponseDTO) => void;
  onExit: () => void;
  onInvalidKey: () => void;
}

export function SignonForm({ onSignonSuccess, onExit, onInvalidKey }: SignonFormProps) {
  const [formData, setFormData] = useState<SignonRequestDTO>({
    userId: '',
    password: ''
  });
  const [message, setMessage] = useState('');
  const [messageType, setMessageType] = useState<'success' | 'error' | 'info'>('info');
  const [loading, setLoading] = useState(false);
  const [cursorField, setCursorField] = useState<'userId' | 'password'>('userId');

  // Handle input changes with COBOL field length validation
  const handleInputChange = (field: keyof SignonRequestDTO, value: string) => {
    let processedValue = value;
    
    if (field === 'userId') {
      // Convert to uppercase and limit to 8 characters (COBOL business logic)
      processedValue = value.toUpperCase().slice(0, 8);
    } else if (field === 'password') {
      // Limit to 8 characters (COBOL business logic)
      processedValue = value.slice(0, 8);
    }

    setFormData(prev => ({
      ...prev,
      [field]: processedValue
    }));

    // Clear message when user starts typing
    if (message) {
      setMessage('');
    }
  };

  // Handle ENTER key press (COSGN00C business logic)
  const handleEnterKey = async () => {
    setLoading(true);
    setMessage('');

    try {
      // Validate inputs using service validation methods
      const userIdValidation = userService.validateUserId(formData.userId);
      if (!userIdValidation.valid) {
        setMessage(userIdValidation.message);
        setMessageType('error');
        setCursorField('userId');
        return;
      }

      const passwordValidation = userService.validatePassword(formData.password);
      if (!passwordValidation.valid) {
        setMessage(passwordValidation.message);
        setMessageType('error');
        setCursorField('password');
        return;
      }

      // Attempt signon
      const response = await userService.signon(formData);
      
      if (response.success) {
        setMessage(response.message);
        setMessageType('success');
        onSignonSuccess(response);
      } else {
        setMessage(response.message);
        setMessageType('error');
        // Position cursor based on error type
        if (response.message.includes('User ID') || response.message.includes('User not found')) {
          setCursorField('userId');
        } else {
          setCursorField('password');
        }
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Authentication failed';
      setMessage(errorMessage);
      setMessageType('error');
      
      // Position cursor based on error message
      if (errorMessage.includes('User ID') || errorMessage.includes('User not found')) {
        setCursorField('userId');
      } else {
        setCursorField('password');
      }
    } finally {
      setLoading(false);
    }
  };

  // Handle PF3 key press (Exit)
  const handlePF3Key = async () => {
    try {
      const response = await userService.exit();
      setMessage(response.message);
      setMessageType('info');
      onExit();
    } catch (error) {
      setMessage('Exit failed');
      setMessageType('error');
    }
  };

  // Handle invalid key press
  const handleInvalidKey = async () => {
    try {
      const response = await userService.handleInvalidKey();
      setMessage(response.message);
      setMessageType('error');
      onInvalidKey();
    } catch (error) {
      setMessage('Invalid key pressed');
      setMessageType('error');
    }
  };

  // Handle form submission
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    handleEnterKey();
  };

  // Handle key press events (simulating COBOL function keys)
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'F3' || (e.altKey && e.key === '3')) {
      e.preventDefault();
      handlePF3Key();
    } else if (e.key === 'F1' || e.key === 'F2' || e.key === 'F4' || e.key === 'F5') {
      e.preventDefault();
      handleInvalidKey();
    }
  };

  return (
    <div className="min-h-screen bg-gray-900 text-green-400 font-mono">
      {/* Header - simulating COBOL screen header */}
      <div className="bg-gray-800 p-4 border-b border-green-400">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">CARDDEMO</h1>
            <h2 className="text-lg">Sign On</h2>
          </div>
          <div className="text-right">
            <div>TRAN: CC00</div>
            <div>PGM: COSGN00C</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })}</div>
            <div>{new Date().toLocaleTimeString('en-US', { 
              hour12: false,
              hour: '2-digit',
              minute: '2-digit',
              second: '2-digit'
            })}</div>
          </div>
        </div>
      </div>

      {/* Main form area */}
      <div className="p-8">
        <form onSubmit={handleSubmit} onKeyDown={handleKeyPress} className="space-y-6">
          {/* User ID field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="userId" className="w-24 text-right">
              User ID:
            </label>
            <input
              id="userId"
              type="text"
              value={formData.userId}
              onChange={(e) => handleInputChange('userId', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'userId' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-32 focus:outline-none focus:border-yellow-400`}
              maxLength={8}
              autoFocus={cursorField === 'userId'}
              disabled={loading}
            />
            <span className="text-gray-500">(Max 8 characters)</span>
          </div>

          {/* Password field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="password" className="w-24 text-right">
              Password:
            </label>
            <input
              id="password"
              type="password"
              value={formData.password}
              onChange={(e) => handleInputChange('password', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'password' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-32 focus:outline-none focus:border-yellow-400`}
              maxLength={8}
              autoFocus={cursorField === 'password'}
              disabled={loading}
            />
            <span className="text-gray-500">(Max 8 characters)</span>
          </div>

          {/* Action buttons - simulating COBOL function keys */}
          <div className="flex space-x-4 mt-8">
            <Button
              type="submit"
              disabled={loading}
              className="bg-green-700 hover:bg-green-600 text-white"
            >
              {loading ? 'Signing On...' : 'ENTER - Sign On'}
            </Button>
            
            <Button
              type="button"
              onClick={handlePF3Key}
              variant="secondary"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white"
            >
              PF3 - Exit
            </Button>
          </div>

          {/* Message area */}
          {message && (
            <div className={`mt-6 p-3 border ${
              messageType === 'success' ? 'border-green-400 text-green-400' :
              messageType === 'error' ? 'border-red-400 text-red-400' :
              'border-yellow-400 text-yellow-400'
            } bg-gray-800`}>
              {message}
            </div>
          )}

          {/* Instructions */}
          <div className="mt-8 text-gray-500 text-sm">
            <p>Instructions:</p>
            <ul className="list-disc list-inside mt-2 space-y-1">
              <li>Enter your User ID and Password</li>
              <li>Press ENTER or click "ENTER - Sign On" to authenticate</li>
              <li>Press F3 or click "PF3 - Exit" to exit</li>
              <li>User ID and Password are limited to 8 characters each</li>
            </ul>
          </div>
        </form>
      </div>
    </div>
  );
}