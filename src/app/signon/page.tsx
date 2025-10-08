'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userSecurityService } from '@/services/userSecurityService';
import { SignonRequestDTO, SignonResponseDTO } from '@/types/userSecurity';

export default function SignonPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<SignonRequestDTO>({
    userId: '',
    password: ''
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [showThankYou, setShowThankYou] = useState(false);

  useEffect(() => {
    // Clear any existing authentication on page load
    userSecurityService.clearAuth();
  }, []);

  const handleInputChange = (field: keyof SignonRequestDTO) => (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const value = e.target.value.slice(0, 8); // Max 8 characters
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
    setError(null);
  };

  const validateForm = (): boolean => {
    if (!formData.userId.trim()) {
      setError('User ID is required');
      return false;
    }
    if (!formData.password.trim()) {
      setError('Password is required');
      return false;
    }
    return true;
  };

  const handleEnter = async () => {
    if (!validateForm()) {
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const response: SignonResponseDTO = await userSecurityService.signon(formData);

      if (response.success && response.user) {
        // Redirect based on user type
        if (response.user.userType === 'ADMIN') {
          router.push('/coadm01c');
        } else if (response.user.userType === 'GENERAL') {
          router.push('/comen01c');
        } else {
          setError('Invalid user type');
        }
      } else {
        setError(response.message || 'Authentication failed');
      }
    } catch (err) {
      if (err instanceof Error) {
        const errorMessage = err.message.toLowerCase();
        if (errorMessage.includes('wrong password')) {
          setError('Wrong Password');
        } else if (errorMessage.includes('user not found')) {
          setError('User not found');
        } else {
          setError('Authentication failed. Please try again.');
        }
      } else {
        setError('An unexpected error occurred');
      }
    } finally {
      setLoading(false);
    }
  };

  const handlePF3Exit = async () => {
    setLoading(true);
    try {
      await userSecurityService.exit();
      setShowThankYou(true);
      setTimeout(() => {
        router.push('/');
      }, 2000);
    } catch (err) {
      setShowThankYou(true);
      setTimeout(() => {
        router.push('/');
      }, 2000);
    } finally {
      setLoading(false);
    }
  };

  const handlePF4Clear = () => {
    setFormData({
      userId: '',
      password: ''
    });
    setError(null);
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    const key = e.key;
    
    // Handle function keys
    if (key === 'F3') {
      e.preventDefault();
      handlePF3Exit();
      return;
    }
    
    if (key === 'F4') {
      e.preventDefault();
      handlePF4Clear();
      return;
    }
    
    if (key === 'Enter') {
      e.preventDefault();
      handleEnter();
      return;
    }

    // Check for invalid keys (non-alphanumeric and not allowed special keys)
    const allowedKeys = [
      'Backspace', 'Delete', 'Tab', 'Escape', 'ArrowLeft', 'ArrowRight', 
      'ArrowUp', 'ArrowDown', 'Home', 'End'
    ];
    
    const isAlphaNumeric = /^[a-zA-Z0-9]$/.test(key);
    const isAllowedSpecial = allowedKeys.includes(key);
    
    if (!isAlphaNumeric && !isAllowedSpecial) {
      e.preventDefault();
      setError('Invalid key pressed');
      
      // Send invalid key notification to server
      userSecurityService.invalidKey().catch(() => {
        // Silent fail for invalid key notification
      });
    }
  };

  if (showThankYou) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="bg-white p-8 rounded-lg shadow-md text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-4">
            Thank You
          </h1>
          <p className="text-gray-600">
            Thank you for using CARDDEMO. Goodbye!
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center">
      <div className="bg-white p-8 rounded-lg shadow-md w-full max-w-md">
        {/* CARDDEMO Title */}
        <div className="text-center mb-8">
          <h1 className="text-3xl font-bold text-blue-600 mb-2">
            CARDDEMO
          </h1>
          <h2 className="text-xl text-gray-700">
            Sign On
          </h2>
        </div>

        {/* Error Message */}
        {error && (
          <div className="mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded">
            {error}
          </div>
        )}

        {/* Sign-on Form */}
        <div className="space-y-6" onKeyDown={handleKeyPress}>
          {/* User ID Field */}
          <div>
            <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-2">
              User ID
            </label>
            <Input
              id="userId"
              type="text"
              value={formData.userId}
              onChange={handleInputChange('userId')}
              maxLength={8}
              placeholder="Enter User ID (max 8 chars)"
              disabled={loading}
              className="w-full"
              autoComplete="username"
            />
          </div>

          {/* Password Field */}
          <div>
            <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-2">
              Password
            </label>
            <Input
              id="password"
              type="password"
              value={formData.password}
              onChange={handleInputChange('password')}
              maxLength={8}
              placeholder="Enter Password (max 8 chars)"
              disabled={loading}
              className="w-full"
              autoComplete="current-password"
            />
          </div>

          {/* Action Buttons */}
          <div className="space-y-3">
            {/* Enter Button */}
            <Button
              onClick={handleEnter}
              disabled={loading || !formData.userId.trim() || !formData.password.trim()}
              className="w-full bg-blue-600 hover:bg-blue-700 text-white"
            >
              {loading ? 'Signing In...' : 'Enter'}
            </Button>

            {/* Function Key Buttons */}
            <div className="grid grid-cols-2 gap-3">
              <Button
                onClick={handlePF3Exit}
                disabled={loading}
                variant="outline"
                className="w-full"
              >
                PF3 Exit
              </Button>
              
              <Button
                onClick={handlePF4Clear}
                disabled={loading}
                variant="outline"
                className="w-full"
              >
                PF4 Clear
              </Button>
            </div>
          </div>
        </div>

        {/* Instructions */}
        <div className="mt-8 text-sm text-gray-600 text-center">
          <p className="mb-2">Instructions:</p>
          <ul className="text-xs space-y-1">
            <li>• Enter your User ID and Password (max 8 characters each)</li>
            <li>• Press Enter or click Enter button to sign in</li>
            <li>• Press F3 or click PF3 Exit to exit</li>
            <li>• Press F4 or click PF4 Clear to clear fields</li>
          </ul>
        </div>

        {/* Loading Indicator */}
        {loading && (
          <div className="mt-4 text-center">
            <div className="inline-block animate-spin rounded-full h-6 w-6 border-b-2 border-blue-600"></div>
          </div>
        )}
      </div>
    </div>
  );
}