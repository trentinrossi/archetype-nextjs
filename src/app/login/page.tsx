'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { Input, Button } from '@/components/ui';

export default function LoginPage() {
  const router = useRouter();
  const [formData, setFormData] = useState({
    userId: '',
    password: '',
  });
  const [errorMessage, setErrorMessage] = useState('');
  const [loading, setLoading] = useState(false);
  const [currentDate, setCurrentDate] = useState('');
  const [currentTime, setCurrentTime] = useState('');

  useEffect(() => {
    if (userService.isAuthenticated()) {
      const user = userService.getCurrentUser();
      if (user?.userType === 'A') {
        router.push('/admin-menu');
      } else {
        router.push('/main-menu');
      }
    }

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [router]);

  const updateDateTime = () => {
    const now = new Date();
    setCurrentDate(now.toLocaleDateString('en-US', { 
      month: '2-digit', 
      day: '2-digit', 
      year: 'numeric' 
    }));
    setCurrentTime(now.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      hour12: false 
    }));
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setErrorMessage('');

    if (!formData.userId.trim()) {
      setErrorMessage('Please enter User ID ...');
      return;
    }

    if (!formData.password.trim()) {
      setErrorMessage('Please enter Password ...');
      return;
    }

    try {
      setLoading(true);
      const response = await userService.login(formData);
      
      if (response.success) {
        if (response.user.userType === 'A') {
          router.push('/admin-menu');
        } else {
          router.push('/main-menu');
        }
      } else {
        setErrorMessage(response.message);
      }
    } catch (error: any) {
      setErrorMessage(error.message || 'Authentication failed. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo Application</h2>
          </div>
          <div className="text-right">
            <div className="text-sm">Date: {currentDate}</div>
            <div className="text-sm">Time: {currentTime}</div>
          </div>
        </div>
      </div>

      <div className="flex-1 flex items-center justify-center p-6">
        <div className="bg-white shadow-lg rounded-lg p-8 w-full max-w-md">
          <div className="mb-6 text-center">
            <h3 className="text-xl font-bold text-gray-800 mb-2">Sign On</h3>
            <div className="text-sm text-gray-600">
              <div>Transaction: COSGN</div>
              <div>Program: COSGN00C</div>
            </div>
          </div>

          {errorMessage && (
            <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-red-700 text-sm">
              {errorMessage}
            </div>
          )}

          <form onSubmit={handleSubmit} className="space-y-4">
            <Input
              label="User ID"
              value={formData.userId}
              onChange={(e) => setFormData({ ...formData, userId: e.target.value })}
              maxLength={8}
              required
              autoFocus
            />

            <Input
              label="Password"
              type="password"
              value={formData.password}
              onChange={(e) => setFormData({ ...formData, password: e.target.value })}
              required
            />

            <div className="pt-4">
              <Button type="submit" disabled={loading} className="w-full">
                {loading ? 'Signing In...' : 'Sign In'}
              </Button>
            </div>
          </form>

          <div className="mt-6 pt-6 border-t border-gray-200 text-center text-sm text-gray-600">
            <div>Application ID: CARDEMO</div>
            <div>System ID: AWS-MAINFRAME-MOD</div>
          </div>
        </div>
      </div>
    </div>
  );
}
